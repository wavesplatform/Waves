package scorex.network

import akka.actor.Actor.Receive
import akka.actor.Cancellable
import scorex.app.{Application, RunnableApplication}
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

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global


class BlockchainSynchronizer(application: Application) extends ViewSynchronizer with ScorexLogging {

  import BlockchainSynchronizer._
  import application.basicMessagesSpecsRepo._

  override val messageSpecs = Seq(SignaturesSpec, BlockMessageSpec)

  protected override lazy val networkControllerRef = application.networkController

  private lazy val coordinator = application.coordinator

  private val gettingBlockTimeout = application.settings.historySynchronizerTimeout
  private val forkMaxLength = application.settings.forkMaxLength
  private val operationAttempts = application.settings.operationAttempts

  private var timeoutData = Option.empty[Cancellable]

  override def receive: Receive = idle

  def idle: Receive = state(Idle) {
    case GetExtension(lastIds, peerScores) =>
      noPeersData("gettingExtension") { _ =>
        val msg = Message(GetSignaturesSpec, Right(lastIds), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(peerScores.keys.toSeq))

        gettingExtension(peerScores)
      }
  }

  def gettingExtension(scores: PeerScores): Receive =
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
            gotoGettingExtensionTail(DownloadInfo(commonBlockId), tail)(PeersData(connectedPeer, scores))
        }
    }

  private def gotoGettingExtensionTail(downloadInfo: DownloadInfo, tail: InnerIds)(implicit peersData: PeersData): Unit = {
    val activePeer = peersData.active
    val blockIdsToDownload = downloadInfo.blockIds ++ tail

    if (blockIdsToDownload.size > forkMaxLength || tail.isEmpty) {
      val fork = blockIdsToDownload.take(forkMaxLength)

      fork.find(id => application.history.contains(id.blockId)) match {
        case Some(suspiciousBlockId) =>
          blacklistPeer(activePeer, s"Suspicious block id: $suspiciousBlockId among blocks to be downloaded")
          gotoIdle(success = false)

        case None =>
          val blocks = mutable.Seq(fork.map(_ -> Option.empty[Block]):_*)

          run("gettingBlocks") { case Some(updated) =>

            val ids = blocks.filter(_._2.isEmpty).map(_._1)

            log.debug(s"Going to request ${ids.size} blocks, peer: ${updated.active}")

            ids.foreach { blockId =>
              val msg = Message(GetBlockSpec, Right(blockId.blockId), None)
              networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(updated.active))
            }

            gettingBlocks(blocks, updated)
          }
      }
    } else {
      val withTail = downloadInfo.copy(blockIds = blockIdsToDownload)
      val overlap = withTail.lastTwoBlockIds

      run("gettingExtensionTail") { case Some(updated) =>
        val msg = Message(GetSignaturesSpec, Right(overlap.reverse.map(_.blockId)), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(updated.active))

        gettingExtensionTail(withTail, overlap, updated)
      }
    }
  }

  def gettingExtensionTail(downloadInfo: DownloadInfo, overlap: InnerIds, peersData: PeersData): Receive =
    state(GettingExtensionTail, senderIn(Set(peersData.active))) {
      case SignaturesFromPeer(tail, connectedPeer) =>

        log.debug(s"Got tail blockIds: $tail")

        if (tail == overlap) {
          gotoGettingExtensionTail(downloadInfo, Seq.empty)(peersData)
        } else if (tail.indexOf(overlap.last) == 0) {
          gotoGettingExtensionTail(downloadInfo, tail.tail)(peersData)
        } else if (tail.lastOption.exists(downloadInfo.blockIds.contains)) {
          log.warn(s"Tail blockIds have been already recieved - possible msg duplication: $tail")
        } else {
          blacklistPeer(connectedPeer, s"Tail does not correspond to the overlap $overlap: $tail")
          gotoIdle(success = false)
        }
    }

  def gettingBlocks(blocks: mutable.Seq[(InnerId, Option[Block])], peersData: PeersData): Receive = {
    object blockIdx {
      def unapply(block: Block): Option[(Block, Int)] = {
        blocks.indexWhere(_._1 == InnerId(block.uniqueId)) match {
          case idx: Int if idx != -1 => Some((block, idx))
          case _ => None
        }
      }
    }
    state(GettingBlocks) {
      case BlockFromPeer(blockIdx(block, index), connectedPeer) if peersData.active == connectedPeer =>
        if (blocks(index)._2.isEmpty) {
          log.info("Got block: " + block.encodedId)

          blocks(index) = InnerId(block.uniqueId) -> Some(block)

          if (blocks.forall(_._2.isDefined)) {
            val expectedScore = peersData.scores(peersData.active)
            // TODO score check! initial vs actual vs expected
            coordinator ! ApplyFork(blocks.flatMap(_._2), connectedPeer)
            gotoIdle(success = true)
          }
        }
    }
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

      case TimeoutExceeded(s, f, peersData, runs) =>
        if (timeoutData.exists(!_.isCancelled)) {
          log.warn(s"Attempt number $runs to rerun $s")
          if (runs < operationAttempts) {
            run(s, peersData, runs + 1) { f(_) }
          } else {
            log.debug("Trying to find a new active peer...")
            val updated = peersData.flatMap {
              case PeersData(active, scores) =>
                blacklistPeer(active, "Timeout exceeded")
                val haveLargerScore = (scores - active).filter(_._2 >= scores(active))
                haveLargerScore.headOption.map {
                  case (peer, _) =>
                    log.debug(s"New active peer is $peer")
                    PeersData(peer, haveLargerScore)
                }
            }

            updated match {
              case Some(updatedPeersData) => run(s) { f(_) } (updatedPeersData)
              case None => gotoIdle(success = false)
            }
          }
        }

      case GetExtension(_, _) => // ignore if not idle

      // the signal to initialize
      case Unit =>

      case nonsense: Any =>
        log.warn(s"Got something strange in ${status.name}: $nonsense")
    }: Receive)
  }

  private def gotoIdle(success: Boolean): Unit = {
    log.debug("Transition to idle")
    cancelPreviousTimeoutCountdown()
    context become idle
    coordinator ! SyncFinished(success)
  }


  private def run(status: String)(f: Option[PeersData] => Receive)(implicit peersData: PeersData): Unit =
    run(status, Some(peersData), 1)(f)

  private def noPeersData(status: String)(f: Option[PeersData] => Receive): Unit = run(status, None, 1)(f)

  private def run(status: String, peersData: Option[PeersData], runs: Int)(f: Option[PeersData] => Receive): Unit = {
    log.debug(s"Transition to $status")
    cancelPreviousTimeoutCountdown()
    val behaviour = f(peersData)
    val timeoutInfo = TimeoutExceeded(status, f, peersData, runs)
    val cancellable = context.system.scheduler.scheduleOnce(gettingBlockTimeout, self, timeoutInfo)
    timeoutData = Some(cancellable)
    context become behaviour
  }

  private def cancelPreviousTimeoutCountdown(): Unit = {
    timeoutData.foreach(_.cancel())
    timeoutData = None
  }

  private def blacklistPeer(connectedPeer: ConnectedPeer, reason: String): Unit = {
    log.warn(s"$reason, peer: $connectedPeer")
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

  type PeerScores = Map[ConnectedPeer, BlockchainScore]

  case class GetExtension(lastBlockIds: BlockIds, peerScores: PeerScores)

  case class InnerId(blockId: BlockId) {
    override def equals(obj: scala.Any): Boolean = {
      import shapeless.syntax.typeable._
      obj.cast[InnerId].exists(_.blockId.sameElements(this.blockId))
    }
    override def hashCode(): Int = blockId.hashCode()
    override def toString: String = encode(blockId)
  }

  type InnerIds = Seq[InnerId]

  private case class TimeoutExceeded(status: String, f: Option[PeersData] => Receive, peersData: Option[PeersData], runs: Integer)

  private[network] def blockIdsToStartDownload(blockIds: InnerIds, history: History): Option[(InnerId, InnerIds)] = {
    val (common, toDownload) = blockIds.span(id => history.contains(id.blockId))
    if (common.nonEmpty) Some((common.last, toDownload)) else None
  }

  private[network] case class DownloadInfo(lastCommon: InnerId, blockIds: InnerIds = Seq.empty) {
    def lastTwoBlockIds: InnerIds = if (blockIds.size > 1) blockIds.takeRight(2) else lastCommon +: blockIds
  }

  private type SenderCheck = ConnectedPeer => Boolean

  private def senderIn(peers: Set[ConnectedPeer]): SenderCheck = peers.contains

  private case class PeersData(active: ConnectedPeer, scores: PeerScores)
}
