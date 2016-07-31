package scorex.network

import akka.actor.Actor
import scorex.app.Application
import scorex.block.Block
import scorex.consensus.mining.BlockGeneratorController.StartGeneration
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
  private val maxPeersToBroadcastBlock = application.settings.maxPeersToBroadcastBlock

  //todo: make configurable
  context.system.scheduler.schedule(1.second, 2.seconds, self, SendCurrentScore)

  blockGenerator ! StartGeneration

  override def receive: Receive = {
    case ConsideredValue(candidates) =>
      val localScore = history.score()

      val peers = candidates.filter(_._2 > localScore)

      if (peers.size < forkResolveQuorumSize) {
        log.debug(s"Quorum to download fork is not reached: ${peers.size} peers but should be $forkResolveQuorumSize")
      } else {
        log.info(s"min networkScore=${peers.minBy(_._2)} > localScore=$localScore")
        val lastIds = history.lastBlockIds(application.settings.MaxRollback)
        blockchainSynchronizer ! GetExtension(lastIds, peers.toMap)
      }

    case SyncFinished(_, result) =>
      scoreObserver ! GetScore
      result foreach {
        case (blocks, from, noMoreBlockIds) =>
          log.info(s"Going to process ${blocks.size} blocks")
          processFork(blocks, from, noMoreBlockIds)
      }

    case AddBlock(block, from) =>
      processSingleBlock(block, from)

    case request @ GetStatus =>
      blockchainSynchronizer forward request

    case SendCurrentScore =>
      val msg = Message(ScoreMessageSpec, Right(application.history.score()), None)
      networkControllerRef ! NetworkController.SendToNetwork(msg, SendToRandom)

    // the signal to initialize
    case Unit =>
  }

  private def processSingleBlock(block: Block, from: Option[ConnectedPeer]): Unit = {
    val parentBlockId = block.referenceField.value
    val local = from.isEmpty

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

      if (!(local || history.lastBlock.uniqueId.sameElements(parentBlockId))) {
        // someone has happened to be faster and added a block or blocks in the ledger before
        log.debug(s"A child for parent of the block already exists, local=$local: ${block.json}")
        return false
      }

      true
    }

    if (isBlockToBeAdded) {
      if (!processNewBlock(block, from, local = true)) {
        // TODO: blacklist here
        log.warn(s"Can't apply single block, local=${from.isEmpty}: ${block.json}")
      }
    }
  }

  private def processFork(blocks: Seq[Block], from: Option[ConnectedPeer], noMoreBlockIds: Boolean): Unit =
    blocks.headOption.map(_.referenceField.value).foreach { lastCommonBlockId =>

      def blacklist() = from.foreach(_.handlerRef ! PeerConnectionHandler.Blacklist)

      val initialScore = history.score()

      val expectedScore = blocks.foldLeft(history.scoreOf(lastCommonBlockId)) {
        (sum, block) =>
          val c = application.consensusModule
          c.cumulativeBlockScore(sum, c.blockScore(block))
      }

      if (expectedScore <= initialScore && noMoreBlockIds) {
        log.warn(s"Expected score ($expectedScore) is less than initial ($initialScore), the fork is rejected")
        blacklist()
      } else {

        log.debug(s"Expected score ($expectedScore) vs initial ($initialScore)")

        val revertedBlocks =
          history.heightOf(lastCommonBlockId).map(history.height() - _).filter(_ > 0).toList.flatMap {
            tailSize =>
              val lastBlocks = history.lastBlocks(tailSize)
              application.blockStorage.removeAfter(lastCommonBlockId)
              lastBlocks
          }

        blocks.find(!processNewBlock(_, None, local = false)).foreach { failedBlock =>
          log.warn(s"Can't apply block: ${failedBlock.json}")
          if (history.lastBlock.uniqueId.sameElements(failedBlock.referenceField.value)) {
            blacklist()
          }
          if (blocks.head == failedBlock && revertedBlocks.nonEmpty) {
            log.warn(s"Return back ${revertedBlocks.size} reverted blocks: ${revertedBlocks.map(_.encodedId)}")
            revertedBlocks.reverse.foreach(application.blockStorage.appendBlock)
          }
        }

        val finalScore = history.score()
        if (finalScore != expectedScore) log.debug(s"Final score ($finalScore) not equal expected ($expectedScore)")
        if (finalScore <= initialScore) log.debug(s"Final score ($finalScore) is less than initial ($initialScore)")
      }
    }

  private def processNewBlock(block: Block, from: Option[ConnectedPeer], local: Boolean): Boolean = Try {
    if (block.isValid) {
      log.info(s"New block(local: $local): ${block.json}")

      from.foreach { peer =>
        val sendingStrategy = if (local) Broadcast else SendToRandomExceptOf(maxPeersToBroadcastBlock, Seq(peer))
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

  case class SyncFinished(success: Boolean, result: Option[(Seq[Block], Option[ConnectedPeer], Boolean)])

  object SyncFinished {
    def unsuccessfully: SyncFinished = SyncFinished(success = false, None)
    def withEmptyResult: SyncFinished = SyncFinished(success = true, None)
  }
}
