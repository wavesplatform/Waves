package scorex.network

import akka.actor.Actor
import scorex.app.Application
import scorex.block.Block
import scorex.consensus.mining.BlockGeneratorController.StartGeneration
import scorex.crypto.encode.Base58.encode
import scorex.network.BlockChainSynchronizer.{GetExtension, GetStatus}
import scorex.network.NetworkController.SendToNetwork
import scorex.network.ScoreObserver.{ConsideredValue, GetScore}
import scorex.network.message.Message
import scorex.transaction.History
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


class Coordinator(application: Application) extends Actor with ScorexLogging {

  import Coordinator._
  import application.basicMessagesSpecsRepo._

  private lazy val scoreObserver = application.scoreObserver
  private lazy val blockChainSynchronizer = application.blockChainSynchronizer
  private lazy val blockGenerator = application.blockGenerator
  private lazy val networkControllerRef = application.networkController

  private lazy val history = application.history

  //todo: make configurable
  context.system.scheduler.schedule(1.second, 2.seconds, self, SendCurrentScore)

  blockGenerator ! StartGeneration

  override def receive: Receive = {
    case ConsideredValue(Some(networkScore: History.BlockchainScore), witnesses) =>
      val localScore = history.score()
      if (networkScore > localScore) {
        log.info(s"networkScore=$networkScore > localScore=$localScore")
        val lastIds = history.lastBlockIds(application.settings.MaxRollback)
        blockChainSynchronizer ! GetExtension(lastIds, witnesses)
      }

    case ConsideredValue(None, _) =>
      log.info("Got no score from outer world")

    case ApplyFork(blocks, fromPeer) if blocks.nonEmpty =>
      log.info(s"Going to process ${blocks.size} blocks")
      processFork(blocks, Some(fromPeer))

    case AddBlock(block, fromPeer) =>
      val parentBlockId = block.referenceField.value
      val local = fromPeer.isEmpty

      def isBlockToBeAdded: Boolean = {
        if (!local && history.contains(block)) {
          // we have already got the block - skip
          return false
        }

        if (!history.contains(parentBlockId)) {
          // the block either has come too early or, if local, too late (e.g. removeAfter happened)
          log.debug(s"Parent of the block is not in the history, local=$local: ${block.json}")
          return false
        }

        if (!history.lastBlock.uniqueId.sameElements(parentBlockId)) {
          // someone has happened to be faster and added a block or blocks in the ledger before
          log.debug(s"A child for parent of the block already exists, local=$local: ${block.json}")
          return false
        }

        true
      }

      if (isBlockToBeAdded) {
        if (!processNewBlock(block, fromPeer, broadcast = true)) {
          // TODO: blacklist here
          log.warn(s"Can't apply single block, local=${fromPeer.isEmpty}: ${block.json}")
        }
      }

    case SyncFinished(_) =>
      scoreObserver ! GetScore

    case request @ GetStatus =>
      blockChainSynchronizer forward request

    case SendCurrentScore =>
      val msg = Message(ScoreMessageSpec, Right(application.history.score()), None)
      networkControllerRef ! NetworkController.SendToNetwork(msg, SendToRandom)

    // the signal to initialize
    case Unit =>
  }

  private def processFork(blocks: Seq[Block], fromPeer: Option[ConnectedPeer]): Unit = {
    val headParent = blocks.head.referenceField.value

    val revertedBlocks =
      history.heightOf(headParent).map(history.height() - _).filter(_ > 0).toList.flatMap {
        tailSize =>
          val tail = history.lastBlocks(tailSize)
          application.blockStorage.removeAfter(headParent)
          tail
      }

    blocks.find(!processNewBlock(_, fromPeer, broadcast = false)).foreach {
      failedBlock =>
        log.warn(s"Can't apply block: ${failedBlock.json}")
        if (blocks.head == failedBlock && revertedBlocks.nonEmpty) {
          log.warn(s"Return back ${revertedBlocks.size} reverted blocks: ${revertedBlocks.map(_.encodedId)}")
          revertedBlocks.reverse.foreach(application.blockStorage.appendBlock)
        }

        if (history.lastBlock.uniqueId.sameElements(failedBlock.referenceField.value)) {
          fromPeer.foreach(_.handlerRef ! PeerConnectionHandler.Blacklist)
        }
    }
  }

  private def processNewBlock(block: Block, fromPeer: Option[ConnectedPeer], broadcast: Boolean): Boolean = Try {
    val local = fromPeer.isEmpty

    if (block.isValid) {
      log.info(s"New block(local: $local): ${block.json}")

      if (broadcast) {
        val sendingStrategy =
          if (local) Broadcast else SendToRandomExceptOf(application.settings.maxPeersToBroadcastBlock, fromPeer.toSeq)
        networkControllerRef ! SendToNetwork(Message(BlockMessageSpec, Right(block), None), sendingStrategy)
      }

      val oldHeight = history.height()
      val oldScore = history.score()
      transactionalModule.blockStorage.appendBlock(block) match {
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

  case object SendCurrentScore

  case class AddBlock(block: Block, generator: Option[ConnectedPeer])

  case class ApplyFork(blocks: Seq[Block], generator: ConnectedPeer)

  case class SyncFinished(success: Boolean)
}
