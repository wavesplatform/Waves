package com.wavesplatform

import java.time.Duration
import java.util.concurrent.atomic.AtomicBoolean

import com.wavesplatform.mining.Miner
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.settings.{BlockchainSettings, WavesSettings}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import scorex.block.Block
import scorex.consensus.TransactionsOrdering
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.ValidationError.{BlockAppendError, GenericError}
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.util.control.NonFatal

object Coordinator extends ScorexLogging {
  def processFork(checkpoint: CheckpointService,
                  history: History,
                  blockchainUpdater: BlockchainUpdater,
                  stateReader: StateReader,
                  utxStorage: UtxPool,
                  time: Time,
                  settings: WavesSettings,
                  miner: Miner,
                  blockchainReadiness: AtomicBoolean)(newBlocks: Seq[Block]): Either[ValidationError, BigInt] = {
    val extension = newBlocks.dropWhile(history.contains)

    def isForkValidWithCheckpoint(lastCommonHeight: Int): Boolean = {
      extension.zipWithIndex.forall(p => checkpoint.isBlockValid(p._1, lastCommonHeight + 1 + p._2))
    }

    extension.headOption.map(_.reference) match {
      case Some(lastCommonBlockId) =>
        if (history.heightOf(lastCommonBlockId).exists(isForkValidWithCheckpoint)) {
          blockchainUpdater.removeAfter(lastCommonBlockId)

          val result = extension.view
            .map(b => b -> appendBlock(checkpoint, history, blockchainUpdater, stateReader, utxStorage, time, settings.blockchainSettings)(b))
            .collectFirst { case (b, Left(e)) => b -> e }
            .fold[Either[ValidationError, BigInt]](Right(history.score())) {
            case (b, e) =>
              log.warn(s"Can't process fork starting with $lastCommonBlockId, error appending block ${b.uniqueId}: $e")
              Left(e)
          }

          result.foreach { _ =>
            miner.lastBlockChanged(history.height(), history.lastBlock.get)
            updateBlockchainReadinessFlag(history, time, blockchainReadiness, settings.minerSettings.intervalAfterLastBlockThenGenerationIsAllowed)
          }

          result
        } else {
          Left(GenericError("Fork contains block that doesn't match checkpoint, declining fork"))
        }

      case None =>
        Left(GenericError("No common block in sequence"))
    }
  }

  def updateBlockchainReadinessFlag(history: History, time: Time, blockchainReadiness: AtomicBoolean, maxBlockchainAge: Duration): Boolean = {
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
      miner.lastBlockChanged(history.height(), newBlock)
    }
    newScore
  }

  private def appendBlock(checkpoint: CheckpointService,
                          history: History,
                          blockchainUpdater: BlockchainUpdater,
                          stateReader: StateReader,
                          utxStorage: UtxPool,
                          time: Time,
                          settings: BlockchainSettings)(block: Block): Either[ValidationError, Unit] = for {
    _ <- Either.cond(checkpoint.isBlockValid(block, history.height() + 1), (),
      BlockAppendError(s"[h = ${history.height() + 1}] is not valid with respect to checkpoint", block))
    _ <- Either.cond(blockConsensusValidation(history, stateReader, settings, time.correctedTime())(block), (),
      BlockAppendError("consensus data is not valid", block))
    _ <- blockchainUpdater.processBlock(block)
  } yield block.transactionData.foreach(utxStorage.remove)

  def processCheckpoint(checkpoint: CheckpointService, history: History, blockchainUpdater: BlockchainUpdater, checkpointPublicKey: ByteStr)
                       (newCheckpoint: Checkpoint): Either[ValidationError, BigInt] =
    if (!checkpoint.get.forall(_.signature sameElements newCheckpoint.signature)) {
      if (EllipticCurveImpl.verify(newCheckpoint.signature, newCheckpoint.toSign, checkpointPublicKey.arr)) {
        checkpoint.set(Some(newCheckpoint))
        makeBlockchainCompliantWith(history, blockchainUpdater)(newCheckpoint)
        Right(history.score())
      } else {
        Left(GenericError("Invalid checkpoint signature"))
      }
    } else {
      Left(GenericError("Checkpoint already applied"))
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

  private def blockConsensusValidation(history: History, state: StateReader, bcs: BlockchainSettings, currentTs: Long)(block: Block): Boolean = try {

    import PoSCalc._

    val fs = bcs.functionalitySettings

    val blockTime = block.timestamp

    require(blockTime - currentTs < MaxTimeDrift, s"Block timestamp $blockTime is from future")

    if (blockTime > fs.requireSortedTransactionsAfter) {
      require(block.transactionData.sorted(TransactionsOrdering.InBlock) == block.transactionData, "Transactions must be sorted correctly")
    }

    val parentOpt = history.parent(block)
    require(parentOpt.isDefined || history.height() == 1,
      s"Can't find parent ${block.reference} of ${block.uniqueId}")

    val parent = parentOpt.get
    val parentHeightOpt = history.heightOf(parent.uniqueId)
    require(parentHeightOpt.isDefined, s"Can't get height of ${block.reference}")
    val parentHeight = parentHeightOpt.get

    val prevBlockData = parent.consensusData
    val blockData = block.consensusData

    val cbt = calcBaseTarget(bcs.genesisSettings.averageBlockDelay, parentHeight, parent, history.parent(parent, 2), blockTime)
    val bbt = blockData.baseTarget
    require(cbt == bbt, s"Declared baseTarget $bbt of ${block.uniqueId} does not match calculated baseTarget $cbt")

    val generator = block.signerData.generator

    //check generation signature
    val calcGs = calcGeneratorSignature(prevBlockData, generator)
    val blockGs = blockData.generationSignature
    require(calcGs.sameElements(blockGs),
      s"Declared signature ${blockGs.mkString} of ${block.uniqueId} does not match calculated signature ${calcGs.mkString}")

    val effectiveBalance = generatingBalance(state, fs)(generator, parentHeight)

    if (blockTime >= fs.minimalGeneratingBalanceAfterTimestamp) {
      require(effectiveBalance >= MinimalEffectiveBalanceForGenerator, s"Effective balance $effectiveBalance is less that minimal ($MinimalEffectiveBalanceForGenerator)")
    }

    //check hit < target
    calcHit(prevBlockData, generator) < calcTarget(parent, blockTime, effectiveBalance)
  } catch {
    case e: IllegalArgumentException =>
      log.error("Error while checking a block", e)
      false
    case NonFatal(t) =>
      log.error("Fatal error while checking a block", t)
      throw t
  }
}
