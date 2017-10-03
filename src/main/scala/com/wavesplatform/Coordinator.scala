package com.wavesplatform

import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.metrics._
import com.wavesplatform.mining.Miner
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.settings.{BlockchainSettings, FunctionalitySettings, WavesSettings}
import com.wavesplatform.state2.reader.StateReader
import com.wavesplatform.state2.ByteStr
import kamon.Kamon
import org.influxdb.dto.Point
import scorex.block.{Block, MicroBlock}
import scorex.consensus.TransactionsOrdering
import scorex.transaction.ValidationError.{GenericError, MicroBlockAppendError}
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.concurrent.duration._
import PoSCalc._

object Coordinator extends ScorexLogging with Instrumented {
  def processFork(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
                  stateReader: StateReader, utxStorage: UtxPool, time: Time, settings: WavesSettings, miner: Miner,
                  blockchainReadiness: AtomicBoolean, featureProvider: FeatureProvider)
                 (newBlocks: Seq[Block]): Either[ValidationError, Option[BigInt]] = {
    val extension = newBlocks.dropWhile(history.contains)

    extension.headOption.map(_.reference) match {
      case Some(lastCommonBlockId) =>

        def isForkValidWithCheckpoint(lastCommonHeight: Int): Boolean =
          extension.zipWithIndex.forall(p => checkpoint.isBlockValid(p._1.signerData.signature, lastCommonHeight + 1 + p._2))

        def forkApplicationResultEi: Either[ValidationError, BigInt] = {
          val firstDeclined = extension.view
            .map { b =>
              b -> appendBlock(
                checkpoint, history, blockchainUpdater, stateReader, utxStorage, time, settings.blockchainSettings,
                featureProvider
              )(b, local = false, isExt = true)
            }
            .collectFirst { case (b, Left(e)) => b -> e }

          firstDeclined.foreach {
            case (declinedBlock, _) =>
              extension.view
                .dropWhile(_ != declinedBlock)
                .foreach(BlockStats.declined(_, BlockStats.BlockType.Ext))
          }

          firstDeclined
            .fold[Either[ValidationError, BigInt]](Right(history.score())) {
            case (b, e) =>
              log.warn(s"Can't process fork starting with $lastCommonBlockId, error appending block ${b.uniqueId}: $e")
              Left(e)
          }
        }

        val initalHeight = history.height()
        for {
          commonBlockHeight <- history.heightOf(lastCommonBlockId).toRight(GenericError("Fork contains no common parent"))
          _ <- Either.cond(isForkValidWithCheckpoint(commonBlockHeight), (), GenericError("Fork contains block that doesn't match checkpoint, declining fork"))
          droppedTransactions <- blockchainUpdater.removeAfter(lastCommonBlockId)
          score <- forkApplicationResultEi
        } yield {
          val depth = initalHeight - commonBlockHeight
          if (depth > 0) {
            Metrics.write(
              Point
                .measurement("rollback")
                .addField("depth", initalHeight - commonBlockHeight)
                .addField("txs", droppedTransactions.size)
            )
          }
          droppedTransactions.foreach(utxStorage.putIfNew)
          miner.scheduleMining()
          updateBlockchainReadinessFlag(history, time, blockchainReadiness, settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed)
          Some(score)
        }
      case None =>
        log.debug("No new blocks found in extension")
        Right(None)
    }
  }


  private def updateBlockchainReadinessFlag(history: History, time: Time, blockchainReadiness: AtomicBoolean, maxBlockchainAge: FiniteDuration): Boolean = {
    val expired = time.correctedTime() - history.lastBlockTimestamp().get < maxBlockchainAge.toMillis
    blockchainReadiness.compareAndSet(expired, !expired)
  }

  def processSingleBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, time: Time,
                         stateReader: StateReader, utxStorage: UtxPool, blockchainReadiness: AtomicBoolean,
                         settings: WavesSettings, featureProvider: FeatureProvider)
                        (newBlock: Block, local: Boolean): Either[ValidationError, Option[BigInt]] = measureSuccessful(blockProcessingTimeStats, {
    if (history.contains(newBlock))
      Right(None)
    else {
      val newScore = for {
        _ <- Either.cond(history.heightOf(newBlock.reference).exists(_ >= history.height() - 1), (), GenericError("Can process either new top block or current top block's competitor"))
        _ <- appendBlock(checkpoint, history, blockchainUpdater, stateReader, utxStorage, time, settings.blockchainSettings, featureProvider)(newBlock, local, isExt = false)
      } yield history.score()

      if (local || newScore.isRight) {
        updateBlockchainReadinessFlag(history, time, blockchainReadiness, settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed)
      }
      newScore.right.map(Some(_))
    }
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
      fp.activationHeight(BlockchainFeatures.SmallerMinimalGeneratingBalance).exists(baseHeight >= _)
        && effectiveBalance >= MinimalEffectiveBalanceForGenerator2, effectiveBalance,
      s"generator's effective balance $effectiveBalance is less that required for generation")

  private def appendBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
                          stateReader: StateReader, utxStorage: UtxPool, time: Time, settings: BlockchainSettings,
                          featureProvider: FeatureProvider)
                         (block: Block, local: Boolean, isExt: Boolean): Either[ValidationError, Unit] = for {
    _ <- Either.cond(checkpoint.isBlockValid(block.signerData.signature, history.height() + 1), (),
      GenericError(s"Block ${block.uniqueId} at height ${history.height() + 1} is not valid w.r.t. checkpoint"))
    _ <- blockConsensusValidation(history, featureProvider, settings, time.correctedTime(), block) { height =>
      PoSCalc.generatingBalance(stateReader, settings.functionalitySettings, block.signerData.generator, height).toEither.left.map(_.toString)
        .flatMap(validateEffectiveBalance(featureProvider, settings.functionalitySettings, block, height))
    }
    height = history.height()
    discardedTxs <- blockchainUpdater.processBlock(block)
  } yield {
    if (local) BlockStats.mined(block, height)
    else if (isExt) BlockStats.applied(block, BlockStats.BlockType.Ext, height)
    else BlockStats.applied(block, BlockStats.BlockType.Broadcast, height)

    utxStorage.removeAll(block.transactionData)
    discardedTxs.foreach(utxStorage.putIfNew)
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
          log.warn(s"Fork detected (length = ${fork.size}), rollback to last valid block id [${lastValidBlock.uniqueId}]")
          blockBlockForkStats.increment()
          blockForkHeightStats.record(fork.size)
          blockchainUpdater.removeAfter(lastValidBlock.uniqueId)
      }
    }
  }

  val MaxTimeDrift: Long = 15.seconds.toMillis

  private def blockConsensusValidation(history: History, fp: FeatureProvider, bcs: BlockchainSettings, currentTs: Long, block: Block)
                                      (genBalance: Int => Either[String, Long]): Either[ValidationError, Unit] = history.read { _ =>

    val fs = bcs.functionalitySettings
    val blockTime = block.timestamp
    val generator = block.signerData.generator

    (for {
      height <- history.heightOf(block.reference).toRight(s"history does not contain parent ${block.reference}")
      _ <- Either.cond(height > fs.blockVersion3After
        || block.version == Block.GenesisBlockVersion
        || block.version == Block.PlainBlockVersion,
        (), s"Block Version 3 can only appear at height greater than ${fs.blockVersion3After}")
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
    } yield ()).left.map(e => GenericError(s"Block ${block.uniqueId} is invalid: $e"))
  }

  private val blockBlockForkStats = Kamon.metrics.counter("block-fork")
  private val blockForkHeightStats = Kamon.metrics.histogram("block-fork-height")
  private val microblockProcessingTimeStats = Kamon.metrics.histogram("microblock-processing-time")
  private val blockProcessingTimeStats = Kamon.metrics.histogram("single-block-processing-time")

}
