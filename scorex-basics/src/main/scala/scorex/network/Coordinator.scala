package scorex.network

import akka.actor.Actor
import scorex.app.Application
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.consensus.mining.BlockGeneratorController.StartGeneration
import scorex.consensus.mining.Miner.GuessABlock
import scorex.crypto.encode.Base58.encode
import scorex.network.BlockchainSynchronizer.{GetExtension, GetStatus}
import scorex.network.NetworkController.SendToNetwork
import scorex.network.ScoreObserver.{ConsideredValue, GetScore}
import scorex.network.message.Message
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


class Coordinator(application: Application) extends Actor with ScorexLogging {

  import Coordinator._
  import application.basicMessagesSpecsRepo._

  private lazy val scoreObserver = application.scoreObserver
  private lazy val blockchainSynchronizer = application.blockchainSynchronizer
  private lazy val blockGenerator = application.blockGenerator
  private lazy val networkControllerRef = application.networkController

  private lazy val history = application.history

  private val forkResolveQuorumSize = application.settings.forkResolveQuorumSize

  context.system.scheduler.schedule(1.second, application.settings.scoreBroadcastDelay, self, BroadcastCurrentScore)

  blockGenerator ! StartGeneration

  override def receive: Receive = idle

  private def idle: Receive = state(CIdle) {
    case ConsideredValue(candidates) =>
      val localScore = history.score()

      val peers = candidates.filter(_._2 > localScore)

      if (peers.isEmpty) {
        log.trace(s"No peers to sync with, local score: $localScore")
      } else if (peers.size < forkResolveQuorumSize) {
        log.debug(s"Quorum to download fork is not reached: ${peers.size} peers but should be $forkResolveQuorumSize")
      } else {
        log.info(s"min networkScore=${peers.minBy(_._2)} > localScore=$localScore")
        val lastIds = history.lastBlockIds(application.settings.MaxRollback)
        blockchainSynchronizer ! GetExtension(lastIds, peers.toMap)
        context become syncing
      }
  }

  private def syncing: Receive = state(CSyncing) {
    case SyncFinished(_, result) =>
      context become idle
      scoreObserver ! GetScore

      result foreach {
        case (lastCommonBlockId, blocks, from) =>
          log.info(s"Going to process a fork")
          processFork(lastCommonBlockId, blocks, from)
      }
  }

  private def state(status: CoordinatorStatus)(logic: Receive): Receive = {
    logic orElse {
      case GetCoordinatorStatus => sender() ! status

      case request @ GetStatus => blockchainSynchronizer forward request

      case AddBlock(block, from) => processSingleBlock(block, from)

      case BroadcastCurrentScore =>
        val msg = Message(ScoreMessageSpec, Right(application.history.score()), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, Broadcast)

      // the signal to initialize
      case Unit =>
    }
  }

  private def processSingleBlock(comingBlock: Block, from: Option[ConnectedPeer]): Unit = {
    val parentBlockId = comingBlock.referenceField.value
    val locallyGenerated = from.isEmpty

    val isBlockToBeAdded = if (history.contains(comingBlock)) {
      // we have already got the block - skip
      false
    } else if (history.contains(parentBlockId)) {

      val lastBlock = history.lastBlock

      if (!lastBlock.uniqueId.sameElements(parentBlockId)) {
        // someone has happened to be faster and already added a block or blocks after the parent
        log.debug(s"A child for parent of the block already exists, local=$locallyGenerated: ${comingBlock.json}")

        val cmp = application.consensusModule.blockOrdering
        if (lastBlock.referenceField.value.sameElements(parentBlockId) && cmp.lt(lastBlock, comingBlock)) {
          log.debug(s"The coming block ${comingBlock.json} is better than last ${lastBlock.json}")
        }

        false

      } else true

    } else {
      // the block either has come too early or, if local, too late (e.g. removeAfter() has come earlier)
      log.debug(s"Parent of the block is not in the history, local=$locallyGenerated: ${comingBlock.json}")
      false
    }

    if (isBlockToBeAdded) {
      if (processNewBlock(comingBlock, from, local = locallyGenerated)) {
        blockGenerator ! GuessABlock(rescheduleImmediately = true)
      } else {
        // TODO: blacklist here
        log.warn(s"Can't apply single block, local=${from.isEmpty}: ${comingBlock.json}")
      }
    }
  }

  private def processFork(lastCommonBlockId: BlockId, blocks: Iterator[Block], from: Option[ConnectedPeer]): Unit = {
    val initialScore = history.score()

    application.blockStorage.removeAfter(lastCommonBlockId)

    blocks.find(!processNewBlock(_, None, local = false)).foreach { failedBlock =>
      log.warn(s"Can't apply block: ${failedBlock.json}")
      if (history.lastBlock.uniqueId.sameElements(failedBlock.referenceField.value)) {
        from.foreach(_.handlerRef ! PeerConnectionHandler.Blacklist)
      }
    }

    val finalScore = history.score()
    if (finalScore < initialScore) log.warn(s"Final score ($finalScore) is less than initial ($initialScore)")
  }

  private def processNewBlock(block: Block, from: Option[ConnectedPeer], local: Boolean): Boolean = Try {
    if (block.isValid) {
      log.info(s"New block(local: $local): ${block.json}")

      if (local) {
        networkControllerRef ! SendToNetwork(Message(BlockMessageSpec, Right(block), None), Broadcast)
      } else {
        self ! BroadcastCurrentScore
      }

      val oldHeight = history.height()
      val oldScore = history.score()
      application.blockStorage.appendBlock(block) match {
        case Success(_) =>
          block.transactionModule.clearFromUnconfirmed(block.transactionDataField.value)
          log.info(
            s"""Block ${block.encodedId} appended:
            (height, score) = ($oldHeight, $oldScore) vs (${history.height()}, ${history.score()})""")
          true
        case Failure(e) =>
          e.printStackTrace()
          log.warn(s"failed to append block: $e")
          false
      }
    } else {
      log.warn(s"Invalid new block(local: $local): ${
        if (log.logger.isDebugEnabled)
          block.json
        else
          encode(block.uniqueId) + ", parent " + encode(block.referenceField.value)}")
      false
    }
  }.getOrElse(false)
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
