package scorex.network

import akka.actor.Actor
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import scorex.app.Application
import scorex.app.Application.GetStatus
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.consensus.mining.BlockGeneratorController.{LastBlockChanged, StartGeneration}
import scorex.crypto.encode.Base58.encode
import scorex.network.BlockchainSynchronizer.{GetExtension, GetSyncStatus, Status}
import scorex.network.NetworkController.SendToNetwork
import scorex.network.ScoreObserver.{CurrentScore, GetScore}
import scorex.network.message.Message
import scorex.network.peer.PeerManager.{ConnectedPeers, GetConnectedPeersTyped}
import scorex.transaction.History.BlockchainScore
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}


class Coordinator(application: Application) extends Actor with ScorexLogging {

  import Coordinator._
  import application.basicMessagesSpecsRepo._

  private lazy val blockchainSynchronizer = application.blockchainSynchronizer
  private lazy val networkControllerRef = application.networkController

  private lazy val history = application.history

  context.system.scheduler.schedule(1.second, application.settings.scoreBroadcastDelay, self, BroadcastCurrentScore)

  application.blockGenerator ! StartGeneration

  override def receive: Receive = idle

  private def idle: Receive = state(CIdle) {
    case CurrentScore(candidates) =>
      val localScore = history.score()

      val betterScorePeers = candidates.filter(_._2 > localScore)

      if (betterScorePeers.isEmpty) {
        log.trace(s"No peers to sync with, local score: $localScore")
      } else {
        log.info(s"min networkScore=${betterScorePeers.minBy(_._2)} > localScore=$localScore")
        application.peerManager ! GetConnectedPeersTyped
        context become syncing(betterScorePeers.toMap)
      }
  }

  private def syncing(peerScores: Map[ConnectedPeer, BlockchainScore]): Receive = state(CSyncing) {
    case ConnectedPeers(peers) =>
      val quorumSize = application.settings.quorum
      if (peers.size < quorumSize) {
        log.debug(s"Quorum to download blocks is not reached: ${peers.size} peers but should be $quorumSize")
        context become idle
      } else {
        blockchainSynchronizer ! GetExtension(peerScores)
      }

    case SyncFinished(_, result) =>
      context become idle
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
        implicit val timeout = Timeout(5 seconds)
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
    }
  }

  private def processSingleBlock(newBlock: Block, from: Option[ConnectedPeer]): Unit = {
    val parentBlockId = newBlock.referenceField.value
    val local = from.isEmpty

    val isBlockToBeAdded = if (history.contains(newBlock)) {
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
    application.blockStorage.removeAfter(lastCommonBlockId)

    blocks.find(!processNewBlock(_)).foreach { failedBlock =>
      log.warn(s"Can't apply block: ${failedBlock.json}")
      if (history.lastBlock.uniqueId.sameElements(failedBlock.referenceField.value)) {
        from.foreach(_.blacklist())
      }
    }

    self ! BroadcastCurrentScore
  }

  private def processNewBlock(block: Block): Boolean = Try {
    if (block.isValid) {
      val oldHeight = history.height()
      val oldScore = history.score()

      application.blockStorage.appendBlock(block) match {
        case Success(_) =>
          block.transactionModule.clearFromUnconfirmed(block.transactionDataField.value)
          log.info(
            s"""Block ${block.encodedId} appended:
            (height, score) = ($oldHeight, $oldScore) vs (${history.height()}, ${history.score()})""")
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
}
