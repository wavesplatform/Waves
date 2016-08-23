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

import scala.concurrent.ExecutionContext.Implicits.global


class BlockchainSynchronizer(application: Application) extends ViewSynchronizer with ScorexLogging {

  import BlockchainSynchronizer._
  import Coordinator.SyncFinished._
  import application.basicMessagesSpecsRepo._

  override val messageSpecs = Seq(SignaturesSpec, BlockMessageSpec)

  protected override lazy val networkControllerRef = application.networkController

  private lazy val coordinator = application.coordinator

  private lazy val timeout = application.settings.historySynchronizerTimeout
  private lazy val forkMaxLength = application.settings.forkMaxLength
  private lazy val operationRetries = application.settings.operationRetries
  private lazy val pinToInitialPeer = application.settings.pinToInitialPeer
  private lazy val partialBlockLoading = !application.settings.loadEntireForkChunk

  private var timeoutData = Option.empty[Cancellable]

  override def receive: Receive = idle

  def idle: Receive = state(Idle) {
    case GetExtension(lastIds, peerScores) =>
      start(GettingExtension) { _ =>
        val msg = Message(GetSignaturesSpec, Right(lastIds), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(peerScores.keys.toSeq))

        gettingExtension(lastIds.map(InnerId), peerScores.map(peer => peer._1 -> Peer(peer._2)))
      }
  }

  def gettingExtension(requestedIds: InnerIds, peers: Peers): Receive =
    state(GettingExtension, acceptSignaturesSpecOnlyFrom(peers.keySet)) {
      case SignaturesFromPeer(blockIds, connectedPeer) =>

        log.info(s"Got blockIds: $blockIds")

        blockIdsToStartDownload(blockIds, application.history) match {
          case None =>
            log.warn(s"Strange blockIds: $blockIds")
            finishUnsuccessfully()

          case Some((_, toDownload)) if toDownload.isEmpty =>
            log.debug(s"All blockIds are already in the local blockchain: $blockIds")
            finish(withEmptyResult)

          case Some((commonBlockId, tail)) if requestedIds.contains(commonBlockId) =>
            implicit val peerSet = PeerSet(connectedPeer,
              if (pinToInitialPeer) peers.filterKeys(_ == connectedPeer) else peers)

            gotoGettingExtensionTail(GettingExtension, DownloadInfo(commonBlockId), tail)

          case Some((commonBlockId, _)) =>
            blacklistPeer(s"Block id: $commonBlockId has not been requested", connectedPeer)
            finishUnsuccessfully()
        }
    }

  private def gotoGettingExtensionTail(initial: Status, downloadInfo: DownloadInfo, tail: InnerIds)
                                      (implicit peers: PeerSet): Unit = {
    val activePeer = peers.active
    val blockIdsToDownload = downloadInfo.blockIds ++ tail

    val noMoreBlockIds = tail.isEmpty
    if (blockIdsToDownload.size >= forkMaxLength || noMoreBlockIds) {
      val fork = blockIdsToDownload.take(forkMaxLength)

      fork.find(id => application.history.contains(id.blockId)) match {
        case Some(suspiciousBlockId) =>
          blacklistPeer(s"Suspicious block id: $suspiciousBlockId among blocks to be downloaded", activePeer)
          finishUnsuccessfully()

        case None =>
          val lastCommonBlockId = downloadInfo.lastCommon.blockId
          val initialScore = application.history.scoreOf(lastCommonBlockId)

          val forkStorage = application.blockStorage.blockSeq
          forkStorage.initialize(fork, initialScore)

          run(initial, GettingBlocks) { updatedPeerData =>
            gettingBlocks(forkStorage, lastCommonBlockId, updatedPeerData)
          }
      }
    } else {
      val withTail = downloadInfo.copy(blockIds = blockIdsToDownload)
      val overlap = withTail.lastTwoBlockIds

      run(initial, GettingExtensionTail) { updatedPeersData =>
        val msg = Message(GetSignaturesSpec, Right(overlap.reverse.map(_.blockId)), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(updatedPeersData.active))

        gettingExtensionTail(withTail, overlap, updatedPeersData)
      }
    }
  }

  def gettingExtensionTail(downloadInfo: DownloadInfo, overlap: InnerIds, peers: PeerSet): Receive =
    state(GettingExtensionTail, acceptSignaturesSpecOnlyFrom(peers.active)) {
      case SignaturesFromPeer(tail, connectedPeer) =>

        log.info(s"Got tail blockIds: $tail")

        if (tail == overlap) {
          gotoGettingExtensionTail(GettingExtensionTail, downloadInfo, Seq.empty)(peers)
        } else if (tail.indexOf(overlap.last) == 0) {
          gotoGettingExtensionTail(GettingExtensionTail, downloadInfo, tail.tail)(peers)
        } else if (tail.lastOption.exists(downloadInfo.blockIds.contains)) {
          log.warn(s"Tail blockIds have been already recieved - possible msg duplication: $tail")
        } else {
          blacklistPeer(s"Tail does not correspond to the overlap $overlap: $tail", connectedPeer)
          finishUnsuccessfully()
        }
    }

  def gettingBlocks(forkStorage: BlockSeq,
                    lastCommonBlockId: BlockId,
                    peers: PeerSet): Receive = {

    val blockIds = forkStorage.allIdsWithoutBlock
    log.info(s"Going to request blocks: ${blockIds.mkString(",")}, peer: ${peers.active}")
    blockIds.foreach { blockId =>
      val msg = Message(GetBlockSpec, Right(blockId.blockId), None)
      networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(peers.active))
    }

    state(GettingBlocks) {
      case BlockFromPeer(block, connectedPeer)
        if peers.active == connectedPeer && forkStorage.containsBlockId(block.uniqueId) =>

        if (forkStorage.addIfNotContained(block)) {
          log.info("Got block: " + block.encodedId)

          val currentScore = application.history.score()
          val forkScore = forkStorage.cumulativeBlockScore

          val author = Some(connectedPeer).filterNot(_ => peers.activeChanged)

          val allBlocksAreLoaded = forkStorage.noIdsWithoutBlock

          if (forkScore > currentScore) {
            if (partialBlockLoading || allBlocksAreLoaded) {
              finish(SyncFinished(success = true, Some(lastCommonBlockId, forkStorage.blocksInOrder, author)))
            }
          } else if (allBlocksAreLoaded) {
            author.foreach {
              blacklistPeer("All blocks are loaded, but still not enough score", _)
            }
            finish(SyncFinished.unsuccessfully)
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

      case t @ TimeoutExceeded(_, _, _, _, _) =>
        if (timeoutData.exists(!_.isCancelled)) handleTimeout(t)

      case GetExtension(_, _) => // ignore if not idle

      // the signal to initialize
      case Unit =>

      case nonsense: Any =>
        log.warn(s"Got something strange in ${status.name}: $nonsense")
    }
  }

  private def handleTimeout(t: TimeoutExceeded): Unit = {
    val TimeoutExceeded(i, n, f, peerSet, runs) = t

    log.debug(s"Attempt #$runs to rerun $n")

    val updated = if (runs < operationRetries) updatedPeerSet(peerSet)
    else {
      log.info(s"Max number of retries ($operationRetries) is reached")
      None
    }

    updated match {
      case Some(updatedPeerSet) =>
        log.info(s"New active peer is ${updatedPeerSet.active}" +
          s" (was ${peerSet.map(_.active.toString).getOrElse("no one")})")

        run(i, n, updated, runs + 1) { f }

      case None => finishUnsuccessfully()
    }
  }

  private def updatedPeerSet(peerSet: Option[PeerSet]): Option[PeerSet] =
    peerSet.flatMap {
      case PeerSet(active, peers, activeChanged) =>

        log.debug("Trying to find a new active peer...")

        val peerData @ Peer(score, retries) = peers(active)
        val updatedRetries = retries + 1

        val updatedPeers = (if (updatedRetries >= application.settings.retriesBeforeBlacklisted) {
          /*
            * TODO: blacklisting by timeout makes sense in case of peer indentification by hostname and nonce,
            * othetwise we have chances to blacklist an innocent peer which have already reastablished
            * its TCP connection using another client TCP port number.
            * if (pinToInitialPeer) blacklistPeer("Timeout exceeded", active)
           */
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

  private def run(initial: Status, next: Status)(f: RepeatableCodeBlock)(implicit peers: PeerSet): Unit =
    run(initial, next, Some(peers), 1)(f)

  private def start(next: Status)(f: RepeatableCodeBlock): Unit = run(Idle, next, None, 1)(f)

  private def run(initial: Status, next: Status, initialPeerSet: Option[PeerSet], runs: Int)
                 (f: RepeatableCodeBlock): Unit = {
    if (initial != next) {
      log.debug(s"Transition from $initial to $next")
    }
    cancelPreviousTimeoutCountdown()
    val behaviour = f(initialPeerSet.orNull)
    val timeoutInfo = TimeoutExceeded(initial, next, f, initialPeerSet, runs)
    val cancellable = context.system.scheduler.schedule(timeout, timeout, self, timeoutInfo)
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

  def blockIdsToStartDownload(blockIds: InnerIds, history: History): Option[(InnerId, InnerIds)] = {
    val (common, toDownload) = blockIds.span(id => history.contains(id.blockId))
    if (common.nonEmpty) Some((common.last, toDownload)) else None
  }

  case class DownloadInfo(lastCommon: InnerId, blockIds: InnerIds = Seq.empty) {
    def lastTwoBlockIds: InnerIds = if (blockIds.size > 1) blockIds.takeRight(2) else lastCommon +: blockIds
  }

  private type StopFilter = (Message.MessageCode, ConnectedPeer) => Boolean
  private def noFilter: StopFilter = (_, _) => false

  private type RepeatableCodeBlock = PeerSet => Receive
  private case class TimeoutExceeded(initial: Status, next: Status, f: RepeatableCodeBlock, peers: Option[PeerSet], runs: Integer)

  private case class Peer(score: BlockchainScore, retries: Int = 0)
  private type Peers = Map[ConnectedPeer, Peer]
  private case class PeerSet(active: ConnectedPeer, peers: Peers, activeChanged: Boolean = false)
}
