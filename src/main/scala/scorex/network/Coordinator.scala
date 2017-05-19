package scorex.network

import akka.actor.{Actor, ActorRef}
import cats._
import cats.implicits._
import com.wavesplatform.network.Network
import com.wavesplatform.settings.{BlockchainSettings, WavesSettings}
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import scorex.block.Block
import scorex.consensus.TransactionsOrdering
import scorex.consensus.mining.BlockGeneratorController.LastBlockChanged
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.encode.Base58.encode
import scorex.transaction.ValidationError.GenericError
import scorex.transaction._
import scorex.utils.{ScorexLogging, Time}

import scala.concurrent.duration._
import scala.language.{higherKinds, postfixOps}
import scala.util.control.NonFatal
import com.wavesplatform.state2._

class Coordinator(network: Network, blockchainSynchronizer: ActorRef, blockGenerator: ActorRef,
                  peerManager: ActorRef, scoreObserver: ActorRef, blockchainUpdater: BlockchainUpdater, time: Time,
                  utxStorage: UnconfirmedTransactionsStorage,
                  history: History, stateReader: StateReader, checkpoints: CheckpointService, settings: WavesSettings) extends Actor with ScorexLogging {

  import Coordinator._



  override def receive: Receive = {
    case _=>
  }

  private def handleCheckpoint(checkpoint: Checkpoint, from: Option[ConnectedPeer]): Unit =
    if (checkpoints.get.forall(c => !(c.signature sameElements checkpoint.signature))) {
      val maybePublicKeyBytes = Base58.decode(settings.checkpointsSettings.publicKey).toOption

      maybePublicKeyBytes foreach {
        publicKey =>
          if (EllipticCurveImpl.verify(checkpoint.signature, checkpoint.toSign, publicKey)) {
            checkpoints.set(Some(checkpoint))
            network.broadcast(checkpoint, from.map(_ => ???)) // todo: don't broadcast to sender
            makeBlockchainCompliantWith(checkpoint)
          } else {
            from.foreach(_.blacklist())
          }
      }
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
          log.warn(s"Fork detected (length = ${fork.size}), rollback to last valid block id [${lastValidBlock.encodedId}]")
          blockchainUpdater.removeAfter(lastValidBlock.uniqueId)
      }
    }
  }

  private def processSingleBlock(newBlock: Block, from: Option[ConnectedPeer]): Unit = {
    val parentBlockId = newBlock.reference
    val local = from.isEmpty

    val isBlockToBeAdded = try {
      if (history.contains(newBlock)) {
        // we have already got the block - skip
        false
      } else if (history.contains(parentBlockId)) {

        val lastBlock = history.lastBlock

        if (lastBlock.uniqueId != parentBlockId) {
          // someone has happened to be faster and already added a block or blocks after the parent
          log.debug(s"A child for parent of the block already exists, local=$local: ${str(newBlock)}")

          val cmp = PoSCalc.blockOrdering(history, stateReader, settings.blockchainSettings.functionalitySettings, time)
          if (lastBlock.reference == parentBlockId && cmp.lt(lastBlock, newBlock)) {
            log.debug(s"New block ${str(newBlock)} is better than last ${str(lastBlock)}")
          }

          false

        } else true

      } else {
        // the block either has come too early or, if local, too late (e.g. removeAfter() has come earlier)
        log.debug(s"Parent of the block is not in the history, local=$local: ${str(newBlock)}")
        false
      }
    } catch {
      case e: UnsupportedOperationException =>
        log.debug(s"DB can't find last block because of unexpected modification")
        false
    }

    if (isBlockToBeAdded) {
      log.info(s"New block(local: $local): ${str(newBlock)}")
      processNewBlock(newBlock) match {
        case Right(_) =>
          blockGenerator ! LastBlockChanged
          if (local) {
            network.broadcast(newBlock)
          } else {
//            self ! BroadcastCurrentScore
          }
        case Left(err) =>
          from.foreach(_.blacklist())
          log.warn(s"Can't apply single block, local=$local: ${str(newBlock)}")
      }
    }
  }

  private def processFork(lastCommonBlockId: ByteStr, blocks: Iterator[Block], from: Option[ConnectedPeer]): Unit = {
    val newBlocks = blocks.toSeq

    def isForkValidWithCheckpoint(lastCommonHeight: Int): Boolean = {
      newBlocks.zipWithIndex.forall(p => isValidWithRespectToCheckpoint(p._1, lastCommonHeight + 1 + p._2))
    }

    if (history.heightOf(lastCommonBlockId).exists(isForkValidWithCheckpoint)) {
      blockchainUpdater.removeAfter(lastCommonBlockId)

      foldM[({type l[α] = Either[(ValidationError, ByteStr), α]})#l, List, Block, Unit](newBlocks.toList, ()) { case ((), block: Block) => processNewBlock(block).left.map((_, block.uniqueId)) } match {
        case Right(_) =>
        case Left(err) =>
          log.error(s"Can't processFork(lastBlockCommonId: $lastCommonBlockId because: ${err._1}")
          if (history.lastBlock.uniqueId == err._2) {
            from.foreach(_.blacklist())
          }
      }

//      self ! BroadcastCurrentScore

    } else {
      from.foreach(_.blacklist())
      log.info(s"Fork contains block that doesn't match checkpoint, refusing fork")
    }
  }

  private def isValidWithRespectToCheckpoint(candidate: Block, estimatedHeight: Int): Boolean =
    !checkpoints.get.exists {
      case Checkpoint(items, _) =>
        val blockSignature = candidate.signerData.signature
        items.exists { case BlockCheckpoint(h, sig) =>
          h == estimatedHeight && (blockSignature != ByteStr(sig))
        }
    }

  private def validateWithRespectToCheckpoint(candidate: Block, estimatedHeight: Int): Either[ValidationError, Unit] = {
    if (isValidWithRespectToCheckpoint(candidate, estimatedHeight))
      Right(())
    else
      Left(GenericError(s"Block ${str(candidate)} [h = $estimatedHeight] is not valid with respect to checkpoint"))
  }

  def isBlockValid(b: Block): Either[ValidationError, Unit] = {
    if (history.contains(b)) Right(())
    else {
      def historyContainsParent = history.contains(b.reference)

      def consensusDataIsValid = blockConsensusValidation(
        history,
        stateReader,
        settings.blockchainSettings,
        time)(b)

      if (!historyContainsParent) Left(GenericError(s"Invalid block ${b.encodedId}: no parent block in history"))
      else if (!Signed.validateSignatures(b).isRight) Left(GenericError(s"Invalid block ${b.encodedId}: signature is not valid"))
      else if (!consensusDataIsValid) Left(GenericError(s"Invalid block ${b.encodedId}: consensus data is not valid"))
      else Right(())
    }
  }


  private def processNewBlock(block: Block): Either[ValidationError, Unit] = for {
    _ <- validateWithRespectToCheckpoint(block, history.height() + 1)
    _ <- isBlockValid(block)
    _ <- blockchainUpdater.processBlock(block)
  } yield {
    block.transactionData.foreach(utxStorage.remove)
    UnconfirmedTransactionsStorage.clearIncorrectTransactions(settings.blockchainSettings.functionalitySettings,
      stateReader, utxStorage, time)
  }


  private def str(block: Block) = {
    if (log.logger.isTraceEnabled) block.json
    else block.uniqueId + ", parent " + block.reference
  }
}

object Coordinator extends ScorexLogging {

  case class AddBlock(block: Block, generator: Option[ConnectedPeer])

  case class BroadcastCheckpoint(checkpoint: Checkpoint)

  case object ClearCheckpoint

  def foldM[G[_], F[_], A, B](fa: F[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G], F: Traverse[F]): G[B] =
    F.foldLeft(fa, G.pure(z))((gb, a) => G.flatMap(gb)(f(_, a)))

  val MaxTimeDrift: FiniteDuration = 15.seconds

  def blockConsensusValidation(history: History, state: StateReader, bcs: BlockchainSettings, time: Time)(block: Block): Boolean = try {

    import PoSCalc._

    val fs = bcs.functionalitySettings

    val blockTime = block.timestamp

    require((blockTime - time.correctedTime()).millis < MaxTimeDrift, s"Block timestamp $blockTime is from future")

    if (blockTime > fs.requireSortedTransactionsAfter) {
      require(block.transactionData.sorted(TransactionsOrdering.InBlock) == block.transactionData, "Transactions must be sorted correctly")
    }

    val parentOpt = history.parent(block)
    require(parentOpt.isDefined || history.height() == 1, s"Can't find parent block with id '${block.reference}' of block '${block.uniqueId}'")

    val parent = parentOpt.get
    val parentHeightOpt = history.heightOf(parent.uniqueId)
    require(parentHeightOpt.isDefined, s"Can't get parent block with id '${block.reference}' height")
    val parentHeight = parentHeightOpt.get

    val prevBlockData = parent.consensusData
    val blockData = block.consensusData

    val cbt = calcBaseTarget(history)(bcs.genesisSettings.averageBlockDelay, parent, blockTime)
    val bbt = blockData.baseTarget
    require(cbt == bbt, s"Block's basetarget is wrong, calculated: $cbt, block contains: $bbt")

    val generator = block.signerData.generator

    //check generation signature
    val calcGs = calcGeneratorSignature(prevBlockData, generator)
    val blockGs = blockData.generationSignature
    require(calcGs.sameElements(blockGs),
      s"Block's generation signature is wrong, calculated: ${
        calcGs.mkString
      }, block contains: ${
        blockGs.mkString
      }")

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
