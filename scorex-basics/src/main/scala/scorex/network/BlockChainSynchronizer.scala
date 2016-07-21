package scorex.network

import scorex.app.Application
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58.encode
import scorex.network.Coordinator.{AddBlock, ApplyFork, SyncFinished}
import scorex.network.NetworkController.DataFromPeer
import scorex.network.message.Message
import scorex.utils.ScorexLogging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext.Implicits.global


class BlockChainSynchronizer(application: Application) extends ViewSynchronizer with ScorexLogging {

  import BlockChainSynchronizer._
  import application.basicMessagesSpecsRepo._

  override val messageSpecs = Seq(SignaturesSpec, BlockMessageSpec)

  protected override lazy val networkControllerRef = application.networkController

  private lazy val coordinator = application.coordinator

  private val gettingBlockTimeout = application.settings.historySynchronizerTimeout
  private val forkChunkSize = application.settings.forkChunkSize

  var timeoutData: Option[(Seq[ConnectedPeer], Long)] = None

  context.system.scheduler.schedule(gettingBlockTimeout, gettingBlockTimeout, self, SelfCheck)

  override def receive: Receive = idle

  def idle: Receive = state(Idle, {
    case GetExtension(lastIds, witnesses) =>

      val msg = Message(GetSignaturesSpec, Right(lastIds), None)
      networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(witnesses))

      gotoGettingExtension(witnesses)
  })

  def gettingExtension(witnesses: Seq[ConnectedPeer]): Receive = state(GettingExtension, {
    case SignaturesFromPeer(blockIds, connectedPeer) if witnesses.contains(connectedPeer) =>

      val common = blockIds.head
      log.debug(s"Got blockIds: ${blockIds.map(encode)}")

      val toDownload = blockIds.tail.filter(b => !application.history.contains(b))
      val commonBlockIdExists = application.history.contains(common)
      if (commonBlockIdExists && toDownload.nonEmpty) {
        gotoGettingBlocks(witnesses, toDownload.map(_ -> None), connectedPeer)
        blockIds.tail.foreach { blockId =>
          val msg = Message(GetBlockSpec, Right(blockId), None)
          val stn = SendToChosen(Seq(connectedPeer))
          networkControllerRef ! NetworkController.SendToNetwork(msg, stn)
        }
      } else {
        if (!commonBlockIdExists) log.warn(s"Strange blockIds: ${blockIds.map(encode)}")
        gotoIdle(success = commonBlockIdExists)
      }

    case SignaturesFromPeer(_, nonWitness) => blacklistPeer(nonWitness)
  })

  def gettingBlocks(witnesses: Seq[ConnectedPeer],
                    blocks: Seq[(BlockId, Option[Block])],
                    fromPeer: ConnectedPeer): Receive = {
    object blockIdx {
      def unapply(block: Block): Option[(Block, Int)] = {
        blocks.indexWhere(_._1.sameElements(block.uniqueId)) match {
          case idx: Int if idx != -1 => Some((block, idx))
          case _ => None
        }
      }
    }

    state(GettingBlocks, {
      case BlockFromPeer(blockIdx(block, index), _) if blocks(index)._2.isEmpty =>

        val blockId = block.uniqueId
        log.info("Got block: " + block.encodedId)

        val updBlocks = blocks.updated(index, blockId -> Some(block))
        val toProcess = updBlocks.map(_._2).takeWhile(_.isDefined)

        val allBlocksInPlace = toProcess.size == updBlocks.size

        if (toProcess.size >= forkChunkSize || allBlocksInPlace) {

          coordinator ! ApplyFork(toProcess.flatten, fromPeer)

          if (allBlocksInPlace)
            gotoIdle(success = true)
          else
            gotoGettingBlocks(witnesses, updBlocks.drop(toProcess.size), fromPeer)

        } else gotoGettingBlocks(witnesses, updBlocks, fromPeer)
    })
  }

  private def state(status: Status, logic: Receive): Receive =
  //combine specific logic with common for all the states
    logic orElse ({
      case GetStatus =>
        sender() ! status

      case BlockFromPeer(block, peer) =>
        coordinator ! AddBlock(block, Some(peer))

      case SelfCheck =>
        timeoutData.find(System.currentTimeMillis() - _._2 > gettingBlockTimeout.toMillis).map(_._1).foreach {
          peersToWatch =>
            peersToWatch.foreach(blacklistPeer)
            timeoutData = None
            gotoIdle(success = false)
        }

      case SignaturesFromPeer(_, _) =>

      case GetExtension(_, _) =>

      // the signal to initialize
      case Unit =>

      case nonsense: Any =>
        log.warn(s"Got something strange in ${status.name}: $nonsense")
    }: Receive)

  private def gotoGettingExtension(witnesses: Seq[ConnectedPeer]): Unit = {
    log.debug("Transition to gettingExtension")
    updateTimeoutData(witnesses)
    context become gettingExtension(witnesses)
  }

  private def gotoGettingBlocks(witnesses: Seq[ConnectedPeer],
                                blocks: Seq[(BlockId, Option[Block])],
                                fromPeer: ConnectedPeer): Unit = {
    log.debug("Transition to gettingBlocks")
    updateTimeoutData(Seq(fromPeer))
    context become gettingBlocks(witnesses, blocks, fromPeer)
  }

  private def gotoIdle(success: Boolean): Unit = {
    log.debug("Transition to synced")
    context become idle
    coordinator ! SyncFinished(success)
  }

  private def updateTimeoutData(peersToWatch: Seq[ConnectedPeer]): Unit = {
    timeoutData = Some(peersToWatch, System.currentTimeMillis())
  }

  private def blacklistPeer(connectedPeer: ConnectedPeer): Unit = {
    connectedPeer.handlerRef ! PeerConnectionHandler.Blacklist
  }

  object BlockFromPeer {
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

  object SignaturesFromPeer {
    def unapply(dataFromPeer: DataFromPeer[_]): Option[(Seq[Block.BlockId], ConnectedPeer)] = {
      if (dataFromPeer.messageType == SignaturesSpec.messageCode) {
        dataFromPeer match {
          case DataFromPeer(msgId, blockIds: Seq[Block.BlockId]@unchecked, connectedPeer) =>
            Some((blockIds, connectedPeer))
          case _ =>
            None
        }
      } else None
    }
  }
}

object BlockChainSynchronizer {

  sealed trait Status {
    val name: String
  }

  case object GettingExtension extends Status {
    override val name = "getting extension"
  }

  case object GettingBlocks extends Status {
    override val name = "getting blocks"
  }

  case object Idle extends Status {
    override val name = "idle"
  }

  case object GetStatus

  case class GetExtension(lastBlockIds: Seq[BlockId], witnesses: Seq[ConnectedPeer])

  case object SelfCheck
}
