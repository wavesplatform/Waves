package scorex.network

import java.net.InetSocketAddress

import akka.actor.Cancellable
import scorex.app.Application
import scorex.block.Block
import scorex.block.Block._
import scorex.crypto.encode.Base58.encode
import scorex.network.Coordinator.{AddBlock, ApplyFork, SyncFinished}
import scorex.network.NetworkController.DataFromPeer
import scorex.network.message.Message
import scorex.transaction.History
import scorex.transaction.History._
import scorex.utils.ScorexLogging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext.Implicits.global


class BlockchainSynchronizer(application: Application) extends ViewSynchronizer with ScorexLogging {

  import BlockchainSynchronizer._
  import application.basicMessagesSpecsRepo._

  override val messageSpecs = Seq(SignaturesSpec, BlockMessageSpec)

  protected override lazy val networkControllerRef = application.networkController

  private lazy val coordinator = application.coordinator

  private val gettingBlockTimeout = application.settings.historySynchronizerTimeout
  private val forkMaxLength = application.settings.forkMaxLength

  var timeoutData: Option[Cancellable] = None

  override def receive: Receive = idle

  def idle: Receive = state(Idle) {
    case GetExtension(lastIds, peerScores) =>

      val msg = Message(GetSignaturesSpec, Right(lastIds), None)
      networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(peerScores.keys.toSeq))

      gotoGettingExtension(peerScores)
  }

  def gettingExtension(scores: Map[InetSocketAddress, BlockchainScore]): Receive =
    state(GettingExtension, senderIn(scores.keySet)) {
      case SignaturesFromPeer(blockIds, connectedPeer) =>

        log.debug(s"Got blockIds: $blockIds")

        blockIdsToStartDownload(blockIds, application.history) match {
          case None =>
            log.warn(s"Strange blockIds: $blockIds")
            gotoIdle(success = false)

          case Some((_, toDownload)) if toDownload.isEmpty =>
            log.debug(s"All blockIds are already in the local blockchain: $blockIds")
            gotoIdle(success = true)

          case Some((commonBlockId, tail)) =>
            val score = scores(connectedPeer.socketAddress)
            gotoGettingExtensionTail(connectedPeer, DownloadInfo(commonBlockId), tail, score)
        }
    }

  def gettingExtensionTail(from: InetSocketAddress,
                           downloadInfo: DownloadInfo,
                           overlap: InnerIds,
                           expectedScore: BlockchainScore): Receive =
    state(GettingExtensionTail, senderIn(Set(from))) {
      case SignaturesFromPeer(tail, connectedPeer) =>

        log.debug(s"Got tail blockIds: $tail")

        if (tail == overlap) {
          gotoGettingExtensionTail(connectedPeer, downloadInfo, Seq.empty, expectedScore)
        } else if (tail.indexOf(overlap.last) == 0) {
          gotoGettingExtensionTail(connectedPeer, downloadInfo, tail.tail, expectedScore)
        } else if (tail.lastOption.exists(downloadInfo.blockIds.contains)) {
          log.warn(s"Tail blockIds have been already recieved - possible msg duplication: $tail")
        } else {
          blacklistPeer(connectedPeer, s"Tail does not correspond to the overlap $overlap: $tail")
          gotoIdle(success = false)
        }
    }

  def gettingBlocks(from: InetSocketAddress,
                    blocks: Seq[(InnerId, Option[Block])],
                    expectedScore: BlockchainScore): Receive = {
    object blockIdx {
      def unapply(block: Block): Option[(Block, Int)] = {
        blocks.indexWhere(_._1 == InnerId(block.uniqueId)) match {
          case idx: Int if idx != -1 => Some((block, idx))
          case _ => None
        }
      }
    }

    state(GettingBlocks) {
      case BlockFromPeer(blockIdx(block, index), connectedPeer) if connectedPeer.socketAddress == from =>
        if (blocks(index)._2.isEmpty) {
          log.info("Got block: " + block.encodedId)

          val updBlocks = blocks.updated(index, InnerId(block.uniqueId) -> Some(block))

          if (updBlocks.forall(_._2.isDefined)) {
            // TODO score check! initial vs actual vs expected
            coordinator ! ApplyFork(updBlocks.flatMap(_._2), connectedPeer)
            gotoIdle(success = true)
          } else gotoGettingBlocks(connectedPeer, updBlocks, expectedScore)
        }
    }
  }

  private type SenderCheck = ConnectedPeer => Boolean

  private def senderIn(peers: Set[InetSocketAddress]) : SenderCheck = {
    peer: ConnectedPeer => peers.contains(peer.socketAddress)
  }

  private def state(status: Status, signaturesFromPeerPredicate: SenderCheck= _ => true)(logic: Receive): Receive = {
    //combine specific logic with common for all the states

    val signaturesFromPeerCheckLogic: Receive = {
      case SignaturesFromPeer(_, fromPeer) if ! signaturesFromPeerPredicate(fromPeer) => // just ignore the block ids
    }

    signaturesFromPeerCheckLogic orElse logic orElse ({
      case GetStatus =>
        sender() ! status

      case BlockFromPeer(block, peer) =>
        coordinator ! AddBlock(block, Some(peer))

      case TimeoutExceeded(peersToWatch) =>
        peersToWatch.foreach(blacklistPeer(_, "Timeout exceeded"))
        gotoIdle(success = false)

      case GetExtension(_, _) => // ignore if not idle

      // the signal to initialize
      case Unit =>

      case nonsense: Any =>
        log.warn(s"Got something strange in ${status.name}: $nonsense")
    }: Receive)
  }

  private def gotoGettingExtension(peerScores: PeerScores): Unit = {
    log.debug("Transition to gettingExtension")
    updateTimeoutData(peerScores.keys.toSeq)
    context become gettingExtension(peerScores.map{ case (peer, score) => peer.socketAddress -> score })
  }

  private def gotoGettingExtensionTail(fromPeer: ConnectedPeer,
                                       downloadInfo: DownloadInfo,
                                       tail: InnerIds,
                                       expectedScore: BlockchainScore): Unit = {

    val blockIdsToDownload = downloadInfo.blockIds ++ tail

    if (blockIdsToDownload.size > forkMaxLength || tail.isEmpty) {
      val fork = blockIdsToDownload.take(forkMaxLength)

      fork.find(id => application.history.contains(id.blockId)) match {
        case Some(suspiciousBlockId) =>
          blacklistPeer(fromPeer, s"Suspicious block id: $suspiciousBlockId among blocks to be downloaded")
          gotoIdle(success = false)

        case None =>
          fork.foreach { blockId =>
            val msg = Message(GetBlockSpec, Right(blockId.blockId), None)
            networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(fromPeer))
          }
          gotoGettingBlocks(fromPeer, fork.map(_ -> None), expectedScore)
      }
    } else {
      log.debug("Transition to gettingExtensionTail")

      val withTail = downloadInfo.copy(blockIds = blockIdsToDownload)

      val overlap = withTail.lastTwoBlockIds
      val msg = Message(GetSignaturesSpec, Right(overlap.reverse.map(_.blockId)), None)
      networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(fromPeer))

      updateTimeoutData(Seq(fromPeer))
      context become gettingExtensionTail(fromPeer.socketAddress, withTail, overlap, expectedScore)
    }
  }

  private def gotoGettingBlocks(fromPeer: ConnectedPeer,
                                blocks: Seq[(InnerId, Option[Block])],
                                expectedScore: BlockchainScore): Unit = {
    log.debug("Transition to gettingBlocks")
    updateTimeoutData(Seq(fromPeer))
    context become gettingBlocks(fromPeer.socketAddress, blocks, expectedScore)
  }

  private def gotoIdle(success: Boolean): Unit = {
    log.debug("Transition to idle")
    cancelPreviousTimeoutCountdown()
    context become idle
    coordinator ! SyncFinished(success)
  }

  private def updateTimeoutData(peersToWatch: Seq[ConnectedPeer]): Unit = {
    cancelPreviousTimeoutCountdown()
    val cancellable = context.system.scheduler.scheduleOnce(gettingBlockTimeout, self, TimeoutExceeded(peersToWatch))
    timeoutData = Some(cancellable)
  }

  private def cancelPreviousTimeoutCountdown(): Unit = {
    timeoutData.foreach(_.cancel())
    timeoutData = None
  }

  private def blacklistPeer(connectedPeer: ConnectedPeer, reason: String): Unit = {
    log.warn(s"$reason, peer: ${connectedPeer.socketAddress}")
    connectedPeer.handlerRef ! PeerConnectionHandler.Blacklist
  }

  private object BlockFromPeer {
    def unapply(dataFromPeer: DataFromPeer[_]): Option[(Block, ConnectedPeer)] = {
      if (dataFromPeer.messageType == BlockMessageSpec.messageCode) {
        dataFromPeer match {
          case DataFromPeer(msgId, block: Block, connectedPeer) if block.cast[Block].isDefined =>
            Some((block, connectedPeer))
          case _ =>
            None
        }
      } else None
    }
  }

  private object SignaturesFromPeer {
    def unapply(dataFromPeer: DataFromPeer[_]): Option[(InnerIds, ConnectedPeer)] = {
      if (dataFromPeer.messageType == SignaturesSpec.messageCode) {
        dataFromPeer match {
          case DataFromPeer(msgId, blockIds: Seq[Block.BlockId]@unchecked, connectedPeer) =>
            Some((blockIds.map(InnerId), connectedPeer))
          case _ =>
            None
        }
      } else None
    }
  }
}

object BlockchainSynchronizer {

  private[network] def blockIdsToStartDownload(blockIds: InnerIds, history: History): Option[(InnerId, InnerIds)] = {
    val (common, toDownload) = blockIds.span(id => history.contains(id.blockId))
    if (common.nonEmpty) Some((common.last, toDownload)) else None
  }

  private[network] case class DownloadInfo(lastCommon: InnerId, blockIds: InnerIds = Seq.empty) {
    def lastTwoBlockIds: InnerIds = if (blockIds.size > 1) blockIds.takeRight(2) else lastCommon +: blockIds
  }

  case class InnerId(blockId: BlockId) {
    override def equals(obj: scala.Any): Boolean = {
      import shapeless.syntax.typeable._
      obj.cast[InnerId].exists(_.blockId.sameElements(this.blockId))
    }
    override def hashCode(): Int = blockId.hashCode()
    override def toString: String = encode(blockId)
  }

  type InnerIds = Seq[InnerId]

  sealed trait Status {
    val name: String
  }

  case object GettingExtension extends Status {
    override val name = "getting extension"
  }

  case object GettingExtensionTail extends Status {
    override val name = "getting extension tail"
  }

  case object GettingBlocks extends Status {
    override val name = "getting blocks"
  }

  case object Idle extends Status {
    override val name = "idle"
  }

  case object GetStatus

  type PeerScores = Map[ConnectedPeer, BlockchainScore]

  case class GetExtension(lastBlockIds: BlockIds, peerScores: PeerScores)

  private case class TimeoutExceeded(peers: Seq[ConnectedPeer])
}
