package scorex.network

import akka.actor.Actor.Receive
import akka.actor.Cancellable
import scorex.app.Application
import scorex.block.Block
import scorex.block.Block._
import scorex.crypto.encode.Base58.encode
import scorex.network.Coordinator.{AddBlock, SyncFinished}
import scorex.network.NetworkController.DataFromPeer
import scorex.network.message.Message
import scorex.transaction.History
import scorex.transaction.History._
import scorex.utils.ScorexLogging
import shapeless.syntax.typeable._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global


class BlockchainSynchronizer(application: Application) extends ViewSynchronizer with ScorexLogging {

  import BlockchainSynchronizer._
  import Coordinator.SyncFinished._
  import application.basicMessagesSpecsRepo._

  override val messageSpecs = Seq(SignaturesSpec, BlockMessageSpec)

  protected override lazy val networkControllerRef = application.networkController

  private lazy val coordinator = application.coordinator

  private val gettingBlockTimeout = application.settings.historySynchronizerTimeout
  private val forkMaxLength = application.settings.forkMaxLength
  private val operationRetries = application.settings.operationRetries
  private val retriesBeforeBlacklisted = application.settings.retriesBeforeBlacklisted
  private val pinToInitialPeer = application.settings.pinToInitialPeer

  private var timeoutData = Option.empty[Cancellable]

  override def receive: Receive = idle

  def idle: Receive = state(Idle) {
    case GetExtension(lastIds, peerScores) =>
      start("gettingExtension") { _ =>
        val msg = Message(GetSignaturesSpec, Right(lastIds), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(peerScores.keys.toSeq))

        gettingExtension(lastIds.map(InnerId), peerScores.map(peer => peer._1 -> Peer(peer._2)))
      }
  }

  def gettingExtension(requestedIds: InnerIds, peers: Peers): Receive =
    state(GettingExtension, acceptSignaturesSpecOnlyFrom(peers.keySet)) {
      case SignaturesFromPeer(blockIds, connectedPeer) =>

        log.debug(s"Got blockIds: $blockIds")

        blockIdsToStartDownload(blockIds, application.history) match {
          case None =>
            log.warn(s"Strange blockIds: $blockIds")
            finishUnsuccessfully()

          case Some((_, toDownload)) if toDownload.isEmpty =>
            log.debug(s"All blockIds are already in the local blockchain: $blockIds")
            finish(withEmptyResult)

          case Some((commonBlockId, tail)) if requestedIds.contains(commonBlockId) =>
            implicit val peerSet = PeerSet(
              connectedPeer, if (pinToInitialPeer) peers.filterKeys(_ == connectedPeer) else peers)

            gotoGettingExtensionTail(DownloadInfo(commonBlockId), tail)

          case Some((commonBlockId, _)) =>
            blacklistPeer(s"Block id: $commonBlockId has not been requested", connectedPeer)
            finishUnsuccessfully()
        }
    }

  private def gotoGettingExtensionTail(downloadInfo: DownloadInfo, tail: InnerIds)(implicit peers: PeerSet): Unit = {
    val activePeer = peers.active
    val blockIdsToDownload = downloadInfo.blockIds ++ tail

    val noMoreBlockIds = tail.isEmpty
    if (blockIdsToDownload.size > forkMaxLength || noMoreBlockIds) {
      val fork = blockIdsToDownload.take(forkMaxLength)

      fork.find(id => application.history.contains(id.blockId)) match {
        case Some(suspiciousBlockId) =>
          blacklistPeer(s"Suspicious block id: $suspiciousBlockId among blocks to be downloaded", activePeer)
          finishUnsuccessfully()

        case None =>
          val blocks = mutable.Seq(fork.map(_ -> Option.empty[Block]):_*)

          run("gettingBlocks") { updatedPeerData =>

            val ids = blocks.filter(_._2.isEmpty).map(_._1)

            log.debug(s"Going to request ${ids.size} blocks, peer: ${updatedPeerData.active}")

            ids.foreach { blockId =>
              val msg = Message(GetBlockSpec, Right(blockId.blockId), None)
              networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(updatedPeerData.active))
            }

            gettingBlocks(blocks, noMoreBlockIds, updatedPeerData)
          }
      }
    } else {
      val withTail = downloadInfo.copy(blockIds = blockIdsToDownload)
      val overlap = withTail.lastTwoBlockIds

      run("gettingExtensionTail") { updatedPeersData =>
        val msg = Message(GetSignaturesSpec, Right(overlap.reverse.map(_.blockId)), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(updatedPeersData.active))

        gettingExtensionTail(withTail, overlap, updatedPeersData)
      }
    }
  }

  def gettingExtensionTail(downloadInfo: DownloadInfo, overlap: InnerIds, peers: PeerSet): Receive =
    state(GettingExtensionTail, acceptSignaturesSpecOnlyFrom(peers.active)) {
      case SignaturesFromPeer(tail, connectedPeer) =>

        log.debug(s"Got tail blockIds: $tail")

        if (tail == overlap) {
          gotoGettingExtensionTail(downloadInfo, Seq.empty)(peers)
        } else if (tail.indexOf(overlap.last) == 0) {
          gotoGettingExtensionTail(downloadInfo, tail.tail)(peers)
        } else if (tail.lastOption.exists(downloadInfo.blockIds.contains)) {
          log.warn(s"Tail blockIds have been already recieved - possible msg duplication: $tail")
        } else {
          blacklistPeer(s"Tail does not correspond to the overlap $overlap: $tail", connectedPeer)
          finishUnsuccessfully()
        }
    }

  def gettingBlocks(blocks: mutable.Seq[(InnerId, Option[Block])],
                    noMoreBlockIds: Boolean,
                    peers: PeerSet): Receive = {
    object blockIdx {
      def unapply(block: Block): Option[(Block, Int)] = {
        blocks.indexWhere(_._1 == InnerId(block.uniqueId)) match {
          case idx: Int if idx != -1 => Some((block, idx))
          case _ => None
        }
      }
    }
    state(GettingBlocks) {
      case BlockFromPeer(blockIdx(block, index), connectedPeer) if peers.active == connectedPeer =>
        if (blocks(index)._2.isEmpty) {
          log.info("Got block: " + block.encodedId)

          blocks(index) = InnerId(block.uniqueId) -> Some(block)

          if (blocks.forall(_._2.isDefined)) {
            val author = Some(connectedPeer).filterNot(_ => peers.activeChanged)
            finish(SyncFinished(success = true, Some(blocks.flatMap(_._2), author, noMoreBlockIds)))
          }
        }
    }
  }

  private def state(status: Status, stopFilter: StopFilter = noFilter)(logic: Receive): Receive = {
    //combine specific logic with common for all the states

    ignoreFor(stopFilter) orElse logic orElse {
      case GetStatus =>
        sender() ! status

      case BlockFromPeer(block, peer) =>
        coordinator ! AddBlock(block, Some(peer))

      case SignaturesFromPeer(_, _) =>

      case t @ TimeoutExceeded(_, _, _, _) =>
        if (timeoutData.exists(!_.isCancelled)) handleTimeout(t)

      case GetExtension(_, _) => // ignore if not idle

      // the signal to initialize
      case Unit =>

      case nonsense: Any =>
        log.warn(s"Got something strange in ${status.name}: $nonsense")
    }
  }

  private def handleTimeout(t: TimeoutExceeded): Unit = {
    val TimeoutExceeded(s, f, peerSet, runs) = t

    log.debug(s"Attempt #$runs to rerun $s")

    val updated = if (runs < operationRetries) updatedPeerSet(peerSet)
    else {
      log.info(s"Max number of retries ($operationRetries) is reached")
      None
    }

    updated match {
      case Some(updatedPeerSet) =>
        log.info(s"New active peer is ${updatedPeerSet.active}" +
          s" (was ${peerSet.map(_.active.toString).getOrElse("no one")})")

        run(s, updated, runs + 1) {
          f
        }

      case None => finishUnsuccessfully()
    }
  }

  private def updatedPeerSet(peerSet: Option[PeerSet]): Option[PeerSet] =
    peerSet.flatMap {
      case PeerSet(active, peers, activeChanged) =>

        log.debug("Trying to find a new active peer...")

        val peerData @ Peer(score, retries) = peers(active)
        val updatedRetries = retries + 1

        val updatedPeers = (if (updatedRetries >= retriesBeforeBlacklisted) {
          if (!activeChanged) blacklistPeer("Timeout exceeded", active)
          peers - active
        } else peers + (active -> peerData.copy(retries = updatedRetries))).filterNot(_._2.score < score)

        val sortedByScore = updatedPeers.toSeq.sortBy(_._2.score).map(_._1)

        sortedByScore.filterNot(_ == active).headOption
          .orElse(sortedByScore.headOption)
          .map(newActive => PeerSet(newActive, updatedPeers, activeChanged || newActive != active))
    }

  private def ignoreFor(stopFilter: StopFilter): Receive = {
    case data @ DataFromPeer(msgId, _, connectedPeer) if stopFilter(msgId, connectedPeer) =>
      log.debug(s"Ignoring data: $data")
  }

  private def acceptSignaturesSpecOnlyFrom(peer: ConnectedPeer): StopFilter =
    acceptSignaturesSpecOnlyFrom(Set(peer))

  private def acceptSignaturesSpecOnlyFrom(peers: Set[ConnectedPeer]): StopFilter = {
    (code, peer) => (!peers.contains(peer)) && SignaturesSpec.messageCode == code
  }

  private def finishUnsuccessfully(): Unit = finish(unsuccessfully)

  private def finish(result: SyncFinished): Unit = {
    log.debug(s"Transition to idle, success == ${result.success}")
    cancelPreviousTimeoutCountdown()
    context become idle
    coordinator ! result
  }

  private def run(status: String)(f: RepeatableCodeBlock)(implicit peers: PeerSet): Unit =
    run(status, Some(peers), 1)(f)

  private def start(status: String)(f: RepeatableCodeBlock): Unit = run(status, None, 1)(f)

  private def run(status: String, initialPeerSet: Option[PeerSet], runs: Int)(f: RepeatableCodeBlock): Unit = {
    log.debug(s"Transition to $status")
    cancelPreviousTimeoutCountdown()
    val behaviour = f(initialPeerSet.orNull)
    val timeoutInfo = TimeoutExceeded(status, f, initialPeerSet, runs)
    val cancellable = context.system.scheduler.schedule(gettingBlockTimeout, gettingBlockTimeout, self, timeoutInfo)
    timeoutData = Some(cancellable)
    context become behaviour
  }

  private def cancelPreviousTimeoutCountdown(): Unit = {
    timeoutData.foreach(_.cancel())
    timeoutData = None
  }

  private def blacklistPeer(reason: String, connectedPeer: ConnectedPeer): Unit = {
    log.warn(s"$reason, blacklisted peer: $connectedPeer")
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

  case class GetExtension(lastBlockIds: BlockIds, peerScores: Map[ConnectedPeer, BlockchainScore])

  case class InnerId(blockId: BlockId) {
    override def equals(obj: Any): Boolean = {
      import shapeless.syntax.typeable._
      obj.cast[InnerId].exists(_.blockId.sameElements(this.blockId))
    }
    override def hashCode(): Int = scala.util.hashing.MurmurHash3.seqHash(blockId)
    override def toString: String = encode(blockId)
  }

  type InnerIds = Seq[InnerId]

  private[network] def blockIdsToStartDownload(blockIds: InnerIds, history: History): Option[(InnerId, InnerIds)] = {
    val (common, toDownload) = blockIds.span(id => history.contains(id.blockId))
    if (common.nonEmpty) Some((common.last, toDownload)) else None
  }

  private[network] case class DownloadInfo(lastCommon: InnerId, blockIds: InnerIds = Seq.empty) {
    def lastTwoBlockIds: InnerIds = if (blockIds.size > 1) blockIds.takeRight(2) else lastCommon +: blockIds
  }

  private type StopFilter = (Message.MessageCode, ConnectedPeer) => Boolean
  private def noFilter: StopFilter = (_, _) => false

  private type RepeatableCodeBlock = PeerSet => Receive
  private case class TimeoutExceeded(status: String, f: RepeatableCodeBlock, peers: Option[PeerSet], runs: Integer)

  private case class Peer(score: BlockchainScore, retries: Int = 0)
  private type Peers = Map[ConnectedPeer, Peer]
  private case class PeerSet(active: ConnectedPeer, peers: Peers, activeChanged: Boolean = false)
}
