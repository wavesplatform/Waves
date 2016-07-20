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
        val lastIds = history.lastBlockIds(application.blockStorage.MaxRollback)
        blockChainSynchronizer ! GetExtension(lastIds, witnesses)
      }

    case ConsideredValue(None, _) =>
      log.info("Got no score from outer world")

    case ApplyFork(blocks, fromPeer) =>
      application.blockStorage.removeAfter(blocks.head.referenceField.value)

      log.info(s"Going to process ${blocks.size} blocks")
      blocks.find(!processNewBlock(_, local = false)).foreach {
        failedBlock =>
          log.warn(s"Can't apply block: ${failedBlock.json}")
          if (history.lastBlock.uniqueId sameElements failedBlock.referenceField.value) {
            fromPeer.handlerRef ! PeerConnectionHandler.Blacklist
          }
      }

    case AddBlock(block, fromPeer) =>
      val parentBlockId = block.referenceField.value
      val nonLocal = fromPeer.isDefined

      if (nonLocal && !history.contains(parentBlockId)) {
        log.debug(s"Parent of the block is not yet in the history: ${block.json}")
      } else if (!(history.lastBlock.uniqueId sameElements parentBlockId)) {
        log.debug(s"A child for parent of the block already exists: ${block.json}")
      } else {
        processNewBlock(block, !nonLocal)
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

  private def processNewBlock(block: Block, local: Boolean): Boolean = Try {
    if (block.isValid) {
      log.info(s"New block(local: $local): ${block.json}")

      if (local) networkControllerRef ! SendToNetwork(Message(BlockMessageSpec, Right(block), None), Broadcast)

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
