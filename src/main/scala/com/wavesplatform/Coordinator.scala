package com.wavesplatform

import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.metrics.{BlockStats, Metrics, TxsInBlockchainStats}
import com.wavesplatform.mining.Miner
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.settings.{BlockchainSettings, WavesSettings}
import com.wavesplatform.state2.{ByteStr, Instrumented}
import com.wavesplatform.state2.reader.StateReader
import kamon.Kamon
import org.influxdb.dto.Point
import scorex.block.{Block, MicroBlock}
import scorex.consensus.TransactionsOrdering
import scorex.transaction.ValidationError.{GenericError, MicroBlockAppendError}
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.concurrent.duration._

object Coordinator extends ScorexLogging with Instrumented {
  def processFork(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, stateReader: StateReader,
                  utxStorage: UtxPool, time: Time, settings: WavesSettings, miner: Miner, blockchainReadiness: AtomicBoolean)
                 (newBlocks: Seq[Block]): Either[ValidationError, BigInt] = {
    val extension = newBlocks.dropWhile(history.contains)

    extension.headOption.map(_.reference) match {
      case Some(lastCommonBlockId) =>

        def isForkValidWithCheckpoint(lastCommonHeight: Int): Boolean =
          extension.zipWithIndex.forall(p => checkpoint.isBlockValid(p._1.signerData.signature, lastCommonHeight + 1 + p._2))

        def forkApplicationResultEi: Either[ValidationError, BigInt] = {
          val firstDeclined = extension.view
            .map(b => b -> appendBlock(checkpoint, history, blockchainUpdater, stateReader, utxStorage, time, settings.blockchainSettings)(b, local = false))
            .collectFirst { case (b, Left(e)) => b -> e }

          firstDeclined.foreach {
            case (declinedBlock, _) => extension.view.dropWhile(_ != declinedBlock).foreach(BlockStats.declined)
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
          score
        }
      case None =>
        log.debug("No new blocks found in extension")
        Right(history.score())
    }
  }


  private def updateBlockchainReadinessFlag(history: History, time: Time, blockchainReadiness: AtomicBoolean, maxBlockchainAge: FiniteDuration): Boolean = {
    val expired = time.correctedTime() - history.lastBlockTimestamp().get < maxBlockchainAge.toMillis
    blockchainReadiness.compareAndSet(expired, !expired)
  }

  def processSingleBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, time: Time,
                         stateReader: StateReader, utxStorage: UtxPool, blockchainReadiness: AtomicBoolean,
                         settings: WavesSettings, miner: Miner)(newBlock: Block, local: Boolean): Either[ValidationError, BigInt] = measureSuccessful(blockProcessingTimeStats, {
    val newScore = for {
      _ <- appendBlock(checkpoint, history, blockchainUpdater, stateReader, utxStorage, time, settings.blockchainSettings)(newBlock, local)
    } yield history.score()

    if (local || newScore.isRight) {
      updateBlockchainReadinessFlag(history, time, blockchainReadiness, settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed)
      miner.scheduleMining()
    }
    newScore
  })

  def processMicroBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, utxStorage: UtxPool)
                       (microBlock: MicroBlock): Either[ValidationError, Unit] = measureSuccessful(microblockProcessingTimeStats, for {
    _ <- Either.cond(checkpoint.isBlockValid(microBlock.totalResBlockSig, history.height() + 1), (),
      MicroBlockAppendError(s"[h = ${history.height() + 1}] is not valid with respect to checkpoint", microBlock))
    _ <- blockchainUpdater.processMicroBlock(microBlock)
  } yield utxStorage.removeAll(microBlock.transactionData))


  private def appendBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
                          stateReader: StateReader, utxStorage: UtxPool, time: Time, settings: BlockchainSettings)
                         (block: Block, local: Boolean): Either[ValidationError, Unit] = for {
    _ <- Either.cond(checkpoint.isBlockValid(block.signerData.signature, history.height() + 1), (),
      GenericError(s"Block ${block.uniqueId} at height ${history.height() + 1} is not valid w.r.t. checkpoint"))
    _ <- blockConsensusValidation(history, stateReader, settings, time.correctedTime())(block)
    height = history.height()
    discardedTxs <- blockchainUpdater.processBlock(block)
  } yield {
    if (local) BlockStats.mined(block, height) else BlockStats.applied(block, height)
    TxsInBlockchainStats.record(block.transactionData.size - discardedTxs.size)

    utxStorage.removeAll(block.transactionData)
    discardedTxs.foreach(utxStorage.putIfNew)
  }

  def processCheckpoint(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater)
                       (newCheckpoint: Checkpoint): Either[ValidationError, BigInt] =
    checkpoint.set(newCheckpoint).map { _ =>
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

  private def blockConsensusValidation(history: History, state: StateReader, bcs: BlockchainSettings, currentTs: Long)
                                      (block: Block): Either[ValidationError, Unit] = history.read { _ =>
    import PoSCalc._

    val fs = bcs.functionalitySettings
    val sortStart = fs.requireSortedTransactionsAfter
    val sortEndHeight = fs.enableMicroblocksAfterHeight
    val blockTime = block.timestamp

    (for {
      _ <- Either.cond(state.height > sortEndHeight || block.version == Block.GenesisBlockVersion || block.version == Block.PlainBlockVersion, (),
        s"Block Version 3 can only appear at height greater than $sortEndHeight")
      _ <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), s"timestamp $blockTime is from future")
      _ <- Either.cond(blockTime < sortStart || state.height > sortEndHeight || block.transactionData.sorted(TransactionsOrdering.InBlock) == block.transactionData,
        (), "transactions are not sorted")
      parent <- history.parent(block).toRight(s"history does not contain parent ${block.reference}")
      parentHeight <- history.heightOf(parent.uniqueId).toRight(s"history does not contain parent ${block.reference}")
      prevBlockData = parent.consensusData
      blockData = block.consensusData
      cbt = calcBaseTarget(bcs.genesisSettings.averageBlockDelay, parentHeight, parent, history.parent(parent, 2), blockTime)
      bbt = blockData.baseTarget
      _ <- Either.cond(cbt == bbt, (), s"declared baseTarget $bbt does not match calculated baseTarget $cbt")
      generator = block.signerData.generator
      calcGs = calcGeneratorSignature(prevBlockData, generator)
      blockGs = blockData.generationSignature.arr
      _ <- Either.cond(calcGs.sameElements(blockGs), (),
        s"declared generation signature ${blockGs.mkString} does not match calculated generation signature ${calcGs.mkString}")
      effectiveBalance <- generatingBalance(state, fs, generator, parentHeight).toEither.left.map(er => GenericError(er.getMessage))
      _ <- Either.cond(blockTime < fs.minimalGeneratingBalanceAfter || effectiveBalance >= MinimalEffectiveBalanceForGenerator, (),
        s"generator's effective balance $effectiveBalance is less that minimal ($MinimalEffectiveBalanceForGenerator)")
      hit = calcHit(prevBlockData, generator)
      target = calcTarget(parent, blockTime, effectiveBalance)
      _ <- Either.cond(hit < target, (), s"calculated hit $hit >= calculated target $target")
    } yield ()).left.map(e => GenericError(s"Block ${block.uniqueId} is invalid: $e"))
  }

  private val blockBlockForkStats = Kamon.metrics.counter("block-fork")
  private val blockForkHeightStats = Kamon.metrics.histogram("block-fork-height")
  private val microblockProcessingTimeStats = Kamon.metrics.histogram("microblock-processing-time")
  private val blockProcessingTimeStats = Kamon.metrics.histogram("single-block-processing-time")

}
