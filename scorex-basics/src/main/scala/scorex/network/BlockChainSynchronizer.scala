package scorex.network

import scorex.app.Application
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58
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

  private val GettingBlockTimeout = application.settings.historySynchronizerTimeout

  var lastUpdate: Option[Long] = None

  context.system.scheduler.schedule(GettingBlockTimeout, GettingBlockTimeout, self, SelfCheck)

  override def receive: Receive = idle

  def idle: Receive = state(Idle, {
    case GetExtension(lastIds, witnesses) =>

      resetLastUpdate()

      val msg = Message(GetSignaturesSpec, Right(lastIds), None)
      networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(witnesses))

      gotoGettingExtension(witnesses)
  })

  def gettingExtension(witnesses: Seq[ConnectedPeer]): Receive = state(GettingExtension, {

    case SignaturesFromPeer(blockIds, connectedPeer)
      if witnesses.contains(connectedPeer) => //todo: ban if non-expected sender

      resetLastUpdate()

      val common = blockIds.head
      log.debug(s"Got blockIds: ${blockIds.map(Base58.encode)}")

      val toDownload = blockIds.tail.filter(b => !application.history.contains(b))
      val commonBlockIdExists = application.history.contains(common)
      if (commonBlockIdExists && toDownload.size > 1) {
        gotoGettingBlocks(witnesses, toDownload.map(_ -> None), connectedPeer)
        blockIds.tail.foreach { blockId =>
          val msg = Message(GetBlockSpec, Right(blockId), None)
          val stn = SendToChosen(Seq(connectedPeer))
          networkControllerRef ! NetworkController.SendToNetwork(msg, stn)
        }
      } else {
        if (!commonBlockIdExists) log.warn(s"Strange blockIds: ${blockIds.map(Base58.encode)}")
        gotoIdle(success = commonBlockIdExists)
      }
  })

  def gettingBlocks(witnesses: Seq[ConnectedPeer],
                    blocks: Seq[(BlockId, Option[Block])],
                    fromPeer: ConnectedPeer): Receive = {
    object idx {
      def unapply(block: Block): Option[(Block, Int)] = {
        blocks.indexWhere(_._1.sameElements(block.uniqueId)) match {
          case idx: Int if idx != -1 => Some((block, idx))
          case _ => None
        }
      }
    }

    state(GettingBlocks, {
      case BlockFromPeer(idx(block, idx), connectedPeer) =>

        resetLastUpdate()

        val blockId = block.uniqueId
        log.info("Got block: " + block.encodedId)

        val updBlocks = blocks.updated(idx, blockId -> Some(block))
        if (idx == 0) {
          val toProcess = updBlocks.takeWhile(_._2.isDefined).map(_._2.get)
          coordinator ! ApplyFork(toProcess, fromPeer)
          if (updBlocks.size > toProcess.size)
            gotoGettingBlocks(witnesses, updBlocks.drop(toProcess.length), fromPeer)
          else
            gotoIdle(success = true)
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
        if (lastUpdate.exists(System.currentTimeMillis() - _ > GettingBlockTimeout.toMillis)) {
          lastUpdate = None
          // todo blacklist
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
    context become gettingExtension(witnesses)
  }

  private def gotoGettingBlocks(witnesses: Seq[ConnectedPeer], blocks: Seq[(BlockId, Option[Block])], fromPeer: ConnectedPeer): Unit = {
    log.debug("Transition to gettingBlocks")
    context become gettingBlocks(witnesses, blocks, fromPeer)
  }

  private def gotoIdle(success: Boolean): Unit = {
    log.debug("Transition to synced")
    context become idle
    coordinator ! SyncFinished(success)
  }

  private def resetLastUpdate(): Unit = {
    lastUpdate = Some(System.currentTimeMillis())
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
