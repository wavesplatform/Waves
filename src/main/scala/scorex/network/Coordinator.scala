package scorex.network

import akka.pattern.{ask, pipe}
import akka.util.Timeout
import scorex.app.Application
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.consensus.mining.BlockGeneratorController.{LastBlockChanged, StartGeneration}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.encode.Base58.encode
import scorex.network.BlockchainSynchronizer.{GetExtension, GetSyncStatus, Status}
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.ScoreObserver.{CurrentScore, GetScore}
import scorex.network.message.{Message, MessageSpec}
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.transaction.History.BlockchainScore
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}


class Coordinator(application: Application) extends ViewSynchronizer with ScorexLogging {

  import Coordinator._
  private val basicMessagesSpecsRepo = application.basicMessagesSpecsRepo
  import basicMessagesSpecsRepo._

  override val messageSpecs = Seq[MessageSpec[_]](CheckpointMessageSpec)

  protected override lazy val networkControllerRef = application.networkController

  private lazy val blockchainSynchronizer = application.blockchainSynchronizer

  private lazy val history = application.history

  private def currentCheckpoint(): Option[Checkpoint] = history.getCheckpoint

  context.system.scheduler.schedule(1.second, application.settings.synchronizationSettings.scoreBroadcastInterval, self, BroadcastCurrentScore)

  application.blockGenerator ! StartGeneration

  override def receive: Receive = idle()

  private def idle(peerScores: Map[ConnectedPeer, BlockchainScore] = Map.empty): Receive = state(CIdle) {
    case CurrentScore(candidates) =>
      val localScore = history.score()

      val betterScorePeers = candidates.filter(_._2 > localScore)

      if (betterScorePeers.isEmpty) {
        log.trace(s"No peers to sync with, local score: $localScore")
      } else {
        log.info(s"min networkScore=${betterScorePeers.minBy(_._2)} > localScore=$localScore")
        application.peerManager ! GetConnectedPeersTyped
        context become idle(betterScorePeers.toMap)
      }

    case ConnectedPeers(peers) =>
      val quorumSize = application.settings.minerSettings.quorum
      val actualSize = peers.intersect(peerScores.keySet).size
      if (actualSize < quorumSize) {
        log.debug(s"Quorum to download blocks is not reached: $actualSize peers but should be $quorumSize")
        context become idle()
      } else if (peerScores.nonEmpty) {
        context become syncing
        blockchainSynchronizer ! GetExtension(peerScores)
      }
  }

  private def syncing: Receive = state(CSyncing) {
    case SyncFinished(_, result) =>
      context become idle()
      application.scoreObserver ! GetScore

      result foreach {
        case (lastCommonBlockId, blocks, from) =>
          log.info(s"Going to process blocks")
          processFork(lastCommonBlockId, blocks, from)
      }
  }

  private def state(status: CoordinatorStatus)(logic: Receive): Receive = {
    logic orElse {
      case GetCoordinatorStatus => sender() ! status

      case GetStatus =>
        implicit val timeout = Timeout(5.seconds)
        (blockchainSynchronizer ? GetSyncStatus).mapTo[Status]
          .map { syncStatus =>
            if (syncStatus == BlockchainSynchronizer.Idle && status == CIdle)
              CIdle.name
            else
              s"${status.name} (${syncStatus.name})" }
          .pipeTo(sender())

      case AddBlock(block, from) => processSingleBlock(block, from)

      case BroadcastCurrentScore =>
        val msg = Message(ScoreMessageSpec, Right(application.history.score()), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, Broadcast)

      case DataFromPeer(msgId, checkpoint: Checkpoint@unchecked, remote) if msgId == CheckpointMessageSpec.messageCode =>
        handleCheckpoint(checkpoint, Some(remote))

      case BroadcastCheckpoint(checkpoint) =>
        handleCheckpoint(checkpoint, None)

      case ConnectedPeers(_) =>

      case ClearCheckpoint => history.setCheckpoint(None)
    }
  }

  private def handleCheckpoint(checkpoint: Checkpoint, from: Option[ConnectedPeer]): Unit =
    if (currentCheckpoint.forall(c => !(c.signature sameElements checkpoint.signature))) {
      val maybePublicKeyBytes = Base58.decode(application.settings.checkpointsSettings.publicKey).toOption

      maybePublicKeyBytes foreach {
        publicKey =>
          if (EllipticCurveImpl.verify(checkpoint.signature, checkpoint.toSign, publicKey)) {
            history.setCheckpoint(Some(checkpoint))
            networkControllerRef ! SendToNetwork(Message(CheckpointMessageSpec, Right(checkpoint), None),
              from.map(BroadcastExceptOf).getOrElse(Broadcast))
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
        !(block.signerDataField.value.signature sameElements sig)
    }

    if (fork.nonEmpty) {
      val genesisBlockHeight = 1
      val hh = existingItems.map(_.height) :+ genesisBlockHeight
      history.blockAt(hh(fork.size)).foreach {
        lastValidBlock =>
          log.warn(s"Fork detected (length = ${fork.size}), rollback to last valid block id [${lastValidBlock.encodedId}]")
          application.blockStorage.removeAfter(lastValidBlock.uniqueId)
      }
    }
  }

  private def processSingleBlock(newBlock: Block, from: Option[ConnectedPeer]): Unit = {
    val parentBlockId = newBlock.referenceField.value
    val local = from.isEmpty

    val isBlockToBeAdded = try {
      if (history.contains(newBlock)) {
        // we have already got the block - skip
        false
      } else if (history.contains(parentBlockId)) {

        val lastBlock = history.lastBlock

        if (!lastBlock.uniqueId.sameElements(parentBlockId)) {
          // someone has happened to be faster and already added a block or blocks after the parent
          log.debug(s"A child for parent of the block already exists, local=$local: ${newBlock.json}")

          val cmp = application.consensusModule.blockOrdering
          if (lastBlock.referenceField.value.sameElements(parentBlockId) && cmp.lt(lastBlock, newBlock)) {
            log.debug(s"New block ${newBlock.json} is better than last ${lastBlock.json}")
          }

          false

        } else true

      } else {
        // the block either has come too early or, if local, too late (e.g. removeAfter() has come earlier)
        log.debug(s"Parent of the block is not in the history, local=$local: ${newBlock.json}")
        false
      }
    } catch {
      case e: UnsupportedOperationException =>
        log.debug(s"DB can't find last block because of unexpected modification")
        false
    }

    if (isBlockToBeAdded) {
      log.info(s"New block(local: $local): ${newBlock.json}")
      if (processNewBlock(newBlock)) {
        application.blockGenerator ! LastBlockChanged
        if (local) {
          networkControllerRef ! SendToNetwork(Message(BlockMessageSpec, Right(newBlock), None), Broadcast)
        } else {
          self ! BroadcastCurrentScore
        }
      } else {
        from.foreach(_.blacklist())
        log.warn(s"Can't apply single block, local=$local: ${newBlock.json}")
      }
    }
  }

  private def processFork(lastCommonBlockId: BlockId, blocks: Iterator[Block], from: Option[ConnectedPeer]): Unit = {
    val newBlocks = blocks.toSeq
    def isForkValidWithCheckpoint(lastCommonHeight: Int): Boolean = {
      newBlocks.zipWithIndex.forall(p => isValidWithRespectToCheckpoint(p._1, lastCommonHeight + 1 + p._2))
    }

    if (application.history.heightOf(lastCommonBlockId).exists(isForkValidWithCheckpoint)) {
      application.blockStorage.removeAfter(lastCommonBlockId)

      newBlocks.find(!processNewBlock(_)).foreach { failedBlock =>
        log.warn(s"Can't apply block: ${failedBlock.json}")
        if (try {
          history.lastBlock.uniqueId.sameElements(failedBlock.referenceField.value)
        } catch {
          case e: UnsupportedOperationException =>
            log.debug(s"DB can't find last block because of unexpected modification")
            false
        }) {
          from.foreach(_.blacklist())
        }
      }

      self ! BroadcastCurrentScore

    } else {
      from.foreach(_.blacklist())
      log.info(s"Fork contains block that doesn't match checkpoint, refusing fork")
    }
  }

  private def isValidWithRespectToCheckpoint(candidate: Block, estimatedHeight: Int): Boolean =
    !currentCheckpoint.exists {
      case Checkpoint(items, _) =>
        val blockSignature = candidate.signerDataField.value.signature
        items.exists { case BlockCheckpoint(h, sig) =>
          h == estimatedHeight && !(blockSignature sameElements sig)
        }
    }

  def isBlockValid(b: Block) : Boolean = {
    if (application.transactionModule.blockStorage.history.contains(b)) true //applied blocks are valid
    else {
      def history = application.transactionModule.blockStorage.history.contains(b.referenceField.value)

      def signature = EllipticCurveImpl.verify(b.signerDataField.value.signature, b.bytesWithoutSignature,
        b.signerDataField.value.generator.publicKey)

      def consensus = application.consensusModule.isValid(b)

      def transaction = application.transactionModule.isValid(b)

      if (!history) log.debug(s"Invalid block ${b.encodedId}: no parent block in history")
      else if (!signature) log.debug(s"Invalid block ${b.encodedId}: signature is not valid")
      else if (!consensus) log.debug(s"Invalid block ${b.encodedId}: consensus data is not valid")
      else if (!transaction) log.debug(s"Invalid block ${b.encodedId}: transaction data is not valid")

      history && signature && consensus && transaction
    }
  }


  private def processNewBlock(block: Block): Boolean = Try {
    val oldHeight = history.height()
    val estimatedHeight = oldHeight + 1
    if (!isValidWithRespectToCheckpoint(block, estimatedHeight)) {
      log.warn(s"Block ${str(block)} [h = $estimatedHeight] is not valid with respect to checkpoint")
      false
    } else if (isBlockValid(block)) {

      val oldScore = history.score()

      application.blockStorage.appendBlock(block) match {
        case Success(_) =>
          log.info(
            s"""Block ${block.encodedId} appended:
            (height, score) = ($oldHeight, $oldScore) vs (${history.height()}, ${history.score()})""")

          application.transactionModule.clearFromUnconfirmed(block.transactionDataField.value)
          true
        case Failure(e) => throw e
      }
    } else {
      log.warn(s"Invalid new block: ${str(block)}")
      false
    }
  } recoverWith { case e =>
    e.printStackTrace()
    log.warn(s"Failed to append new block ${str(block)}: $e")
    Failure(e)
  } getOrElse false

  private def str(block: Block) = {
    if (log.logger.isDebugEnabled) block.json
    else encode(block.uniqueId) + ", parent " + encode(block.referenceField.value)
  }
}

object Coordinator {

  case object GetCoordinatorStatus

  sealed trait CoordinatorStatus {
    val name: String
  }

  case object CIdle extends CoordinatorStatus {
    override val name = "idle"
  }

  case object CSyncing extends CoordinatorStatus {
    override val name = "syncing"
  }

  case class AddBlock(block: Block, generator: Option[ConnectedPeer])

  case class SyncFinished(success: Boolean, result: Option[(BlockId, Iterator[Block], Option[ConnectedPeer])])

  object SyncFinished {
    def unsuccessfully: SyncFinished = SyncFinished(success = false, None)
    def withEmptyResult: SyncFinished = SyncFinished(success = true, None)
  }

  private case object BroadcastCurrentScore

  case object GetStatus

  case class BroadcastCheckpoint(checkpoint: Checkpoint)

  case object ClearCheckpoint
}
