package com.wavesplatform

import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.metrics._
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, WavesSettings}
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.SnapshotStateReader
import kamon.Kamon
import monix.eval.Coeval
import org.influxdb.dto.Point
import scorex.block.{Block, MicroBlock}
import scorex.consensus.TransactionsOrdering
import scorex.transaction.PoSCalc._
import scorex.transaction.ValidationError.{GenericError, MicroBlockAppendError}
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.concurrent.duration._
import scala.util.{Left, Right}

object Coordinator extends ScorexLogging with Instrumented {
  def processFork(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
                  stateReader: StateReader, utxStorage: UtxPool, time: Time, settings: WavesSettings,
                  blockchainReadiness: AtomicBoolean, featureProvider: FeatureProvider)
                 (blocks: Seq[Block]): Either[ValidationError, Option[BigInt]] =  Signed.validateOrdered(blocks).flatMap { newBlocks =>
   history.write { implicit l =>
    val extension = newBlocks.dropWhile(history.contains)

    extension.headOption.map(_.reference) match {
      case Some(lastCommonBlockId) =>

        def isForkValidWithCheckpoint(lastCommonHeight: Int): Boolean =
          extension.zipWithIndex.forall(p => checkpoint.isBlockValid(p._1.signerData.signature, lastCommonHeight + 1 + p._2))

        val forkApplicationResultEi = Coeval.evalOnce {
          val firstDeclined = extension.view.map { b =>
            b -> appendBlock(checkpoint, history, blockchainUpdater, stateReader(), utxStorage, time, settings.blockchainSettings, featureProvider)(b).right.map {
              _.foreach(bh => BlockStats.applied(b, BlockStats.Source.Ext, bh))
            }
          }
            .zipWithIndex
            .collectFirst { case ((b, Left(e)), i) => (i, b, e) }

          firstDeclined.foreach {
            case (_, declinedBlock, _) =>
              extension.view
                .dropWhile(_ != declinedBlock)
                .foreach(BlockStats.declined(_, BlockStats.Source.Ext))
          }

          firstDeclined
            .foldLeft[Either[ValidationError, BigInt]](Right(history.score())) {
            case (_, (i, b, e)) if i == 0 =>
              log.warn(s"Can't process fork starting with $lastCommonBlockId, error appending block $b: $e")
              Left(e)

            case (r, (i, b, e)) =>
              log.debug(s"Processed $i of ${newBlocks.size} blocks from extension")
              log.warn(s"Can't process fork starting with $lastCommonBlockId, error appending block $b: $e")
              r
          }
        }

        val initalHeight = history.height()

        val droppedBlocksEi = (for {
          commonBlockHeight <- history.heightOf(lastCommonBlockId).toRight(GenericError("Fork contains no common parent"))
          _ <- Either.cond(isForkValidWithCheckpoint(commonBlockHeight), (), GenericError("Fork contains block that doesn't match checkpoint, declining fork"))
          droppedBlocks <- blockchainUpdater.removeAfter(lastCommonBlockId)
        } yield (commonBlockHeight, droppedBlocks)).left.map((_, Seq.empty[Block]))

        (for {
          commonHeightAndDroppedBlocks <- droppedBlocksEi
          (commonBlockHeight, droppedBlocks) = commonHeightAndDroppedBlocks
          score <- forkApplicationResultEi().left.map((_, droppedBlocks))
        } yield (commonBlockHeight, droppedBlocks, score))
          .right.map { case ((commonBlockHeight, droppedBlocks, score)) =>
          val depth = initalHeight - commonBlockHeight
          if (depth > 0) {
            Metrics.write(
              Point
                .measurement("rollback")
                .addField("depth", initalHeight - commonBlockHeight)
                .addField("txs", droppedBlocks.size)
            )
          }
          droppedBlocks.flatMap(_.transactionData).foreach(utxStorage.putIfNew)
          updateBlockchainReadinessFlag(history, time, blockchainReadiness, settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed)
          Some(score)
        }.left.map { case ((err, droppedBlocks)) =>
          droppedBlocks.foreach(blockchainUpdater.processBlock(_).explicitGet())
          err
        }
      case None =>
        log.debug("No new blocks found in extension")
        Right(None)
    }
  }
  }



  def updateBlockchainReadinessFlag(history: History, time: Time, blockchainReadiness: AtomicBoolean, maxBlockchainAge: FiniteDuration): Boolean = {
    val expired = time.correctedTime() - history.lastBlockTimestamp().get < maxBlockchainAge.toMillis
    blockchainReadiness.compareAndSet(expired, !expired)
  }

  def processSingleBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, time: Time,
                         stateReader: StateReader, utxStorage: UtxPool,
                         settings: BlockchainSettings, featureProvider: FeatureProvider)
                        (newBlock: Block): Either[ValidationError, Option[BigInt]] = measureSuccessful(blockProcessingTimeStats, history.write { implicit l =>
    if (history.contains(newBlock)) Right(None)
    else for {
      _ <- Either.cond(history.heightOf(newBlock.reference).exists(_ >= history.height() - 1), (), GenericError("Can process either new top block or current top block's competitor"))
      maybeBaseHeight <- appendBlock(checkpoint, history, blockchainUpdater, stateReader(), utxStorage, time, settings, featureProvider)(newBlock)
    } yield maybeBaseHeight map (_ => history.score())
  })

  def processMicroBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, utxStorage: UtxPool)
                       (microBlock: MicroBlock): Either[ValidationError, Unit] = measureSuccessful(microblockProcessingTimeStats, for {
    _ <- Either.cond(checkpoint.isBlockValid(microBlock.totalResBlockSig, history.height() + 1), (),
      MicroBlockAppendError(s"[h = ${history.height() + 1}] is not valid with respect to checkpoint", microBlock))
    _ <- blockchainUpdater.processMicroBlock(microBlock)
  } yield utxStorage.removeAll(microBlock.transactionData))

  private def validateEffectiveBalance(fp: FeatureProvider, fs: FunctionalitySettings, block: Block, baseHeight: Int)(effectiveBalance: Long): Either[String, Long] =
    Either.cond(block.timestamp < fs.minimalGeneratingBalanceAfter ||
      (block.timestamp >= fs.minimalGeneratingBalanceAfter && effectiveBalance >= MinimalEffectiveBalanceForGenerator1) ||
      fp.featureActivationHeight(BlockchainFeatures.SmallerMinimalGeneratingBalance.id).exists(baseHeight >= _)
        && effectiveBalance >= MinimalEffectiveBalanceForGenerator2, effectiveBalance,
      s"generator's effective balance $effectiveBalance is less that required for generation")

  private def appendBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
                          stateReader: SnapshotStateReader, utxStorage: UtxPool, time: Time, settings: BlockchainSettings,
                          featureProvider: FeatureProvider)(block: Block): Either[ValidationError, Option[Int]] = for {
    _ <- Either.cond(checkpoint.isBlockValid(block.signerData.signature, history.height() + 1), (),
      GenericError(s"Block $block at height ${history.height() + 1} is not valid w.r.t. checkpoint"))
    _ <- blockConsensusValidation(history, featureProvider, settings, time.correctedTime(), block) { height =>
      PoSCalc.generatingBalance(stateReader, settings.functionalitySettings, block.signerData.generator, height).toEither.left.map(_.toString)
        .flatMap(validateEffectiveBalance(featureProvider, settings.functionalitySettings, block, height))
    }
    baseHeight = history.height()
    maybeDiscardedTxs <- blockchainUpdater.processBlock(block)
  } yield {
    utxStorage.removeAll(block.transactionData)
    maybeDiscardedTxs.toSeq.flatten.foreach(utxStorage.putIfNew)
    maybeDiscardedTxs.map(_ => baseHeight)
  }

  def processCheckpoint(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater)
                       (newCheckpoint: Checkpoint): Either[ValidationError, BigInt] =
    checkpoint.set(newCheckpoint).map { _ =>
      log.info(s"Processing checkpoint $checkpoint")
      makeBlockchainCompliantWith(history, blockchainUpdater)(newCheckpoint)
      history.score()
    }


  private def makeBlockchainCompliantWith(history: History, blockchainUpdater: BlockchainUpdater)(checkpoint: Checkpoint): Unit = {
    val existingItems = checkpoint.items.filter {
      checkpoint => history.blockAt(checkpoint.height).isDefined
    }

    val fork = existingItems.takeWhile {
      case BlockCheckpoint(h, sig) =>
        val block = history.blockAt(h).get
        block.signerData.signature != ByteStr(sig)
    }

    if (fork.nonEmpty) {
      val genesisBlockHeight = 1
      val hh = existingItems.map(_.height) :+ genesisBlockHeight
      history.blockAt(hh(fork.size)).foreach {
        lastValidBlock =>
          log.warn(s"Fork detected (length = ${fork.size}), rollback to last valid block $lastValidBlock]")
          blockBlockForkStats.increment()
          blockForkHeightStats.record(fork.size)
          blockchainUpdater.removeAfter(lastValidBlock.uniqueId)
      }
    }
  }

  val MaxTimeDrift: Long = 100 // millis

  private def blockConsensusValidation(history: History, fp: FeatureProvider, bcs: BlockchainSettings, currentTs: Long, block: Block)
                                      (genBalance: Int => Either[String, Long]): Either[ValidationError, Unit] = history.read { _ =>

    val fs = bcs.functionalitySettings
    val blockTime = block.timestamp
    val generator = block.signerData.generator

    (for {
      height <- history.heightOf(block.reference).toRight(s"history does not contain parent ${block.reference}")
      _ <- Either.cond(height > fs.blockVersion3AfterHeight
        || block.version == Block.GenesisBlockVersion
        || block.version == Block.PlainBlockVersion,
        (), s"Block Version 3 can only appear at height greater than ${fs.blockVersion3AfterHeight}")
      _ <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), s"timestamp $blockTime is from future")
      _ <- Either.cond(blockTime < fs.requireSortedTransactionsAfter
        || height > fs.dontRequireSortedTransactionsAfter
        || block.transactionData.sorted(TransactionsOrdering.InBlock) == block.transactionData,
        (), "transactions are not sorted")
      parent <- history.parent(block).toRight(s"history does not contain parent ${block.reference}")
      prevBlockData = parent.consensusData
      blockData = block.consensusData
      cbt = calcBaseTarget(bcs.genesisSettings.averageBlockDelay, height, parent.consensusData.baseTarget, parent.timestamp, history.parent(parent, 2).map(_.timestamp), blockTime)
      bbt = blockData.baseTarget
      _ <- Either.cond(cbt == bbt, (), s"declared baseTarget $bbt does not match calculated baseTarget $cbt")
      calcGs = calcGeneratorSignature(prevBlockData, generator)
      blockGs = blockData.generationSignature.arr
      _ <- Either.cond(calcGs.sameElements(blockGs), (), s"declared generation signature ${blockGs.mkString} does not match calculated generation signature ${calcGs.mkString}")
      effectiveBalance <- genBalance(height)
      hit = calcHit(prevBlockData, generator)
      target = calcTarget(parent.consensusData.baseTarget, parent.timestamp, blockTime, effectiveBalance)
      _ <- Either.cond(hit < target, (), s"calculated hit $hit >= calculated target $target")
    } yield ()).left.map(e => GenericError(s"Block $block is invalid: $e"))
  }

  private val blockBlockForkStats = Kamon.metrics.counter("block-fork")
  private val blockForkHeightStats = Kamon.metrics.histogram("block-fork-height")
  private val microblockProcessingTimeStats = Kamon.metrics.histogram("microblock-processing-time")
  private val blockProcessingTimeStats = Kamon.metrics.histogram("single-block-processing-time")

}
