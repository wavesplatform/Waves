package com.wavesplatform

import java.time.Duration
import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.mining.Miner
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.settings.{BlockchainSettings, WavesSettings}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import scorex.block.{Block}
import scorex.consensus.TransactionsOrdering
import scorex.transaction.ValidationError.{BlockAppendError, GenericError}
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

object Coordinator extends ScorexLogging {
  def processFork(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, stateReader: StateReader,
                  utxStorage: UtxPool, time: Time, settings: WavesSettings, miner: Miner, blockchainReadiness: AtomicBoolean)
                 (newBlocks: Seq[Block]): Either[ValidationError, BigInt] = {
    val extension = newBlocks.dropWhile(history.contains)

    extension.headOption.map(_.reference) match {
      case Some(lastCommonBlockId) =>

        def isForkValidWithCheckpoint(lastCommonHeight: Int): Boolean =
          extension.zipWithIndex.forall(p => checkpoint.isBlockValid(p._1.signerData.signature, lastCommonHeight + 1 + p._2))

        def forkApplicationResultEi: Either[ValidationError, BigInt] = extension.view
          .map(b => b -> appendBlock(checkpoint, history, blockchainUpdater, stateReader, utxStorage, time, settings.blockchainSettings)(b))
          .collectFirst { case (b, Left(e)) => b -> e }
          .fold[Either[ValidationError, BigInt]](Right(history.score())) {
          case (b, e) =>
            log.warn(s"Can't process fork starting with $lastCommonBlockId, error appending block ${b.uniqueId}: $e")
            Left(e)
        }

        for {
          commonBlockHeight <- history.heightOf(lastCommonBlockId).toRight(GenericError("Fork contains no common parent"))
          _ <- Either.cond(isForkValidWithCheckpoint(commonBlockHeight), (), GenericError("Fork contains block that doesn't match checkpoint, declining fork"))
          _ <- blockchainUpdater.removeAfter(lastCommonBlockId)
          score <- forkApplicationResultEi
        } yield {
          miner.lastBlockChanged()
          updateBlockchainReadinessFlag(history, time, blockchainReadiness, settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed)
          score
        }
      case None =>
        log.debug("No new blocks found in extension")
        Right(history.score())
    }
  }


  private def updateBlockchainReadinessFlag(history: History, time: Time, blockchainReadiness: AtomicBoolean, maxBlockchainAge: Duration): Boolean = {
    val expired = time.correctedTime() - history.lastBlock.get.timestamp < maxBlockchainAge.toMillis
    blockchainReadiness.compareAndSet(expired, !expired)
  }

  def processBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, time: Time,
                   stateReader: StateReader, utxStorage: UtxPool, blockchainReadiness: AtomicBoolean, miner: Miner,
                   settings: WavesSettings)(newBlock: Block, local: Boolean): Either[ValidationError, BigInt] = {
    val newScore = for {
      _ <- appendBlock(checkpoint, history, blockchainUpdater, stateReader, utxStorage, time, settings.blockchainSettings)(newBlock)
    } yield history.score()

    if (local || newScore.isRight) {
      updateBlockchainReadinessFlag(history, time, blockchainReadiness, settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed)
      miner.lastBlockChanged()
    }
    newScore
  }

  private def appendBlock(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater,
                          stateReader: StateReader, utxStorage: UtxPool, time: Time, settings: BlockchainSettings)
                         (block: Block): Either[ValidationError, Unit] = for {
    _ <- Either.cond(checkpoint.isBlockValid(block.signerData.signature, history.height() + 1), (),
      BlockAppendError(s"[h = ${history.height() + 1}] is not valid with respect to checkpoint", block))
    _ <- blockConsensusValidation(history, stateReader, settings, time.correctedTime())(block)
    _ <- blockchainUpdater.processBlock(block)
  } yield block.transactionData.foreach(utxStorage.remove)

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
          blockchainUpdater.removeAfter(lastValidBlock.uniqueId)
      }
    }
  }

  val MaxTimeDrift: Long = Duration.ofSeconds(15).toMillis

  private def blockConsensusValidation(history: History, state: StateReader, bcs: BlockchainSettings, currentTs: Long)
                                      (block: Block): Either[ValidationError, Unit] = {

    import PoSCalc._

    val fs = bcs.functionalitySettings
    val (sortStart, sortEnd) = (fs.requireSortedTransactionsAfter, Long.MaxValue)
    val blockTime = block.timestamp

    (for {
      _ <- Either.cond(blockTime - currentTs < MaxTimeDrift, (), "Block timestamp $blockTime is from future")
      _ <- Either.cond(blockTime < sortStart || blockTime > sortEnd || block.transactionData.sorted(TransactionsOrdering.InBlock) == block.transactionData,
        (), "Transactions must be sorted correctly")
      parent <- history.parent(block).toRight(s"Can't find parent ${block.reference} of ${block.uniqueId}")
      parentHeight <- history.heightOf(parent.uniqueId).toRight(s"Can't get height of ${block.reference}")
      prevBlockData = parent.consensusData
      blockData = block.consensusData
      cbt = calcBaseTarget(bcs.genesisSettings.averageBlockDelay, parentHeight, parent, history.parent(parent, 2), blockTime)
      bbt = blockData.baseTarget
      _ <- Either.cond(cbt == bbt, (), s"Declared baseTarget $bbt of ${block.uniqueId} does not match calculated baseTarget $cbt")
      generator = block.signerData.generator
      calcGs = calcGeneratorSignature(prevBlockData, generator)
      blockGs = blockData.generationSignature
      _ <- Either.cond(calcGs.sameElements(blockGs), (),
        s"Declared signature ${blockGs.mkString} of ${block.uniqueId} does not match calculated signature ${calcGs.mkString}")
      effectiveBalance = generatingBalance(state, fs, generator, parentHeight)
      _ <- Either.cond(blockTime < fs.minimalGeneratingBalanceAfter || effectiveBalance > MinimalEffectiveBalanceForGenerator, (),
        s"Effective balance $effectiveBalance is less that minimal ($MinimalEffectiveBalanceForGenerator)")
      _ <- Either.cond(calcHit(prevBlockData, generator) < calcTarget(parent, blockTime, effectiveBalance), (), "consensus data is not valid")
    } yield ()).left.map(BlockAppendError(_, block))
  }

}
