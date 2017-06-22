package com.wavesplatform

import java.time.Duration

import com.wavesplatform.mining.Miner
import com.wavesplatform.network.{BlockCheckpoint, Checkpoint}
import com.wavesplatform.settings.BlockchainSettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.consensus.TransactionsOrdering
import scorex.crypto.EllipticCurveImpl
import scorex.transaction.ValidationError.{GenericError, InvalidSignature}
import scorex.transaction._
import scorex.utils.ScorexLogging

import scala.util.control.NonFatal

class Coordinator(
                     checkpoint: CheckpointService,
                     history: History,
                     blockchainUpdater: BlockchainUpdater,
                     stateReader: StateReader,
                     utxStorage: UtxPool,
                     time: => Long,
                     settings: BlockchainSettings,
                     maxBlockchainAge: Duration,
                     checkpointPublicKey: ByteStr,
                     miner: Miner,
                     blockchainExpiryListener: Boolean => Unit) extends ScorexLogging {

  import Coordinator._

  private def checkExpiry(): Unit =
    blockchainExpiryListener(time - history.lastBlock.timestamp < maxBlockchainAge.toMillis)

  private def isValidWithRespectToCheckpoint(candidate: Block, estimatedHeight: Int): Boolean =
    !checkpoint.get.exists {
      case Checkpoint(items, _) =>
        val blockSignature = candidate.signerData.signature
        items.exists { case BlockCheckpoint(h, sig) =>
          h == estimatedHeight && blockSignature != ByteStr(sig)
        }
    }

  private def validateWithRespectToCheckpoint(candidate: Block, estimatedHeight: Int): Either[ValidationError, Unit] = {
    if (isValidWithRespectToCheckpoint(candidate, estimatedHeight))
      Right(())
    else
      Left(GenericError(s"Block ${candidate.uniqueId} [h = $estimatedHeight] is not valid with respect to checkpoint"))
  }

  private def isBlockValid(b: Block): Either[ValidationError, Unit] = {
    if (history.contains(b)) Right(())
    else {
      def historyContainsParent = history.contains(b.reference)

      def consensusDataIsValid = blockConsensusValidation(history, stateReader, settings, time)(b)

      if (!historyContainsParent) Left(BlockAppendError("no parent block in history", b))
      else if (!b.signatureValid) Left(InvalidSignature(b, None))
      else if (!consensusDataIsValid) Left(BlockAppendError("consensus data is not valid", b))
      else Right(())
    }
  }

  private def appendBlock(block: Block): Either[ValidationError, Unit] = for {
    _ <- validateWithRespectToCheckpoint(block, history.height() + 1)
    _ <- isBlockValid(block)
    _ <- blockchainUpdater.processBlock(block)
  } yield block.transactionData.foreach(utxStorage.remove)

  def processFork(lastCommonBlockId: BlockId, newBlocks: Seq[Block]): Either[ValidationError, BigInt] = {

    def isForkValidWithCheckpoint(lastCommonHeight: Int): Boolean = {
      newBlocks.zipWithIndex.forall(p => isValidWithRespectToCheckpoint(p._1, lastCommonHeight + 1 + p._2))
    }

    if (history.heightOf(lastCommonBlockId).exists(isForkValidWithCheckpoint)) {
      blockchainUpdater.removeAfter(lastCommonBlockId)

      val result = newBlocks.view.map(b => b -> appendBlock(b)).collectFirst {
        case (b, Left(e)) => b -> e
      }.fold[Either[ValidationError, BigInt]](Right(history.score())) {
        case (b, e) =>
          log.warn(s"Can't process fork starting with $lastCommonBlockId, error appending block ${b.uniqueId}: $e")
          Left(e)
      }

      result.foreach { _ =>
        miner.lastBlockChanged(history.height(), history.lastBlock)
        checkExpiry()
      }

      result
    } else {
      Left(GenericError("Fork contains block that doesn't match checkpoint, declining fork"))
    }
  }

  def processLocalBlock(newBlock: Block): Either[ValidationError, BigInt] = {
    log.debug(s"Processing new local block ${newBlock.uniqueId} (history last block: ${history.lastBlock.uniqueId})")
    val result = processBlock(newBlock)
    // even if newly generated local block could not have been appended, miner will
    // schedule next generation attempt.

    result.left.foreach(_ => miner.lastBlockChanged(history.height(), history.lastBlock))
    result
  }

  def processBlock(newBlock: Block): Either[ValidationError, BigInt] = {
    val blockCanBeAdded = if (newBlock.reference != history.lastBlock.uniqueId) {
      Left(GenericError(s"Parent ${newBlock.reference} does not match local block ${history.lastBlock.uniqueId}"))
    } else if (history.contains(newBlock)) {
      Left(GenericError(s"Block ${newBlock.uniqueId} is already in blockchain"))
    } else Right(newBlock)

    val newScore = for {
      b <- blockCanBeAdded
      _ <- appendBlock(b)
    } yield history.score()

    newScore.foreach { _ =>
      checkExpiry()
      miner.lastBlockChanged(history.height(), newBlock)
    }
    newScore
  }

  def processCheckpoint(newCheckpoint: Checkpoint): Either[ValidationError, BigInt] =
    if (checkpoint.get.forall(_.signature sameElements newCheckpoint.signature)) {
      if (EllipticCurveImpl.verify(newCheckpoint.signature, newCheckpoint.toSign, checkpointPublicKey.arr)) {
        checkpoint.set(Some(newCheckpoint))
        makeBlockchainCompliantWith(newCheckpoint)
        Right(history.score())
      } else {
        Left(GenericError("Invalid checkpoint signature"))
      }
    } else {
      Left(GenericError("Checkpoint already applied"))
    }

  def processRollback(blockId: ByteStr): Either[ValidationError, BigInt] = {
    if (blockchainUpdater.removeAfter(blockId))
      Right(history.score())
    else
      Left(GenericError(s"Failed to rollback to non existing block $blockId"))
  }

  private def makeBlockchainCompliantWith(checkpoint: Checkpoint): Unit = {
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
}

object Coordinator extends ScorexLogging {
  val MaxTimeDrift: Long = Duration.ofSeconds(15).toMillis

  def blockConsensusValidation(history: History, state: StateReader, bcs: BlockchainSettings, currentTs: Long)(block: Block): Boolean = try {

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
