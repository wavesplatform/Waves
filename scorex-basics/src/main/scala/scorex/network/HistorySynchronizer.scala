package scorex.network

import akka.actor.Props
import scorex.app.Application
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.ScoreObserver.{ConsideredValue, UpdateScore}
import scorex.network.message.Message
import scorex.transaction.History
import scorex.utils.ScorexLogging
import shapeless.Typeable._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}


//todo: write tests
class HistorySynchronizer(application: Application) extends ViewSynchronizer with ScorexLogging {

  import HistorySynchronizer._
  import application.basicMessagesSpecsRepo._

  private implicit val consensusModule = application.consensusModule
  private implicit val transactionalModule = application.transactionModule

  override val messageSpecs = Seq(ScoreMessageSpec, SignaturesSpec, BlockMessageSpec)

  private lazy val scoreObserver = context.actorOf(Props(classOf[ScoreObserver], self))

  private lazy val history = application.history

  protected override lazy val networkControllerRef = application.networkController

  private lazy val blockGenerator = application.blockGenerator

  //todo: make configurable
  private val GettingExtensionTimeout = 10.seconds
  private val GettingBlockTimeout = 10.seconds

  override def preStart: Unit = {
    super.preStart()
    //todo: make configurable
    context.system.scheduler.schedule(1.second, 2.seconds) {
      val msg = Message(ScoreMessageSpec, Right(history.score()), None)
      networkControllerRef ! NetworkController.SendToNetwork(msg, SendToRandom)
    }
  }

  override def receive: Receive =
    if (application.settings.offlineGeneration) gotoSynced() else gotoSyncing()

  def state(status: Status, logic: Receive): Receive =
  //combine specific logic with common for all the states
    logic orElse ({
      case HistorySynchronizer.GetStatus =>
        sender() ! status.name

      //todo: check sender
      case DataFromPeer(msgId, score: History.BlockchainScore, connectedPeer)
        if msgId == ScoreMessageSpec.messageCode =>

        scoreObserver ! UpdateScore(Some(connectedPeer -> score))

      case ConsideredValue(Some(networkScore: History.BlockchainScore), witnesses) =>

      case ConsideredValue(None, _) =>
        gotoSyncing()

      //the signal to initialize
      case Unit =>

      case nonsense: Any =>
        log.warn(s"Got something strange: $nonsense")
    }: Receive)

  def syncing: Receive = state(HistorySynchronizer.Syncing, {
    case ConsideredValue(Some(networkScore: History.BlockchainScore), witnesses) =>
      val localScore = history.score()
      if (networkScore > localScore) {
        log.info(s"networkScore=$networkScore > localScore=$localScore")
        val lastIds = history.lastBlocks(100).map(_.uniqueId)
        val msg = Message(GetSignaturesSpec, Right(lastIds), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(witnesses))
        gotoGettingExtension(networkScore, witnesses)
      } else gotoSynced()
  }: Receive)

  def gettingExtension(betterScore: BigInt, witnesses: Seq[ConnectedPeer]): Receive = state(HistorySynchronizer.GettingExtension, {
    case GettingExtensionTimeout => gotoSyncing()

    //todo: aggregating function for block ids (like score has)
    case DataFromPeer(msgId, blockIds: Seq[Block.BlockId]@unchecked, connectedPeer)
      if msgId == SignaturesSpec.messageCode &&
        blockIds.cast[Seq[Block.BlockId]].isDefined &&
        witnesses.contains(connectedPeer) => //todo: ban if non-expected sender

      val common = blockIds.head
      log.debug(s"Got blockIds: ${blockIds.map(id => Base58.encode(id))}")

      val toDownload = blockIds.tail.filter(b => !application.history.contains(b))
      if (application.history.contains(common) && toDownload.nonEmpty) {
        Try(application.blockStorage.removeAfter(common)) //todo we don't need this call for blockTree
        gotoGettingBlock(witnesses, toDownload.map(_ -> None))
        blockIds.tail.foreach { blockId =>
          val msg = Message(GetBlockSpec, Right(blockId), None)
          val stn = SendToChosen(Seq(connectedPeer))
          networkControllerRef ! NetworkController.SendToNetwork(msg, stn)
        }
      } else {
        log.warn(s"Strange blockIds: $blockIds(${application.history.contains(common)})")
        gotoSyncing()
      }
  }: Receive)

  def gettingBlock(witnesses: Seq[ConnectedPeer], blocks: Seq[(BlockId, Option[Block])]): Receive =
    state(HistorySynchronizer.GettingBlock, {
      case GettingBlockTimeout =>
        gotoSyncing()

      case DataFromPeer(msgId, block: Block@unchecked, connectedPeer)
        if msgId == BlockMessageSpec.messageCode && block.cast[Block].isDefined =>

        val blockId = block.uniqueId
        log.info("Got block: " + block.encodedId)

        blocks.indexWhere(_._1.sameElements(blockId)) match {
          case i: Int if i == -1 => gotoGettingBlock(witnesses, blocks)
          case idx: Int =>
            val updBlocks = blocks.updated(idx, blockId -> Some(block))
            if (idx == 0) {
              val toProcess = updBlocks.takeWhile(_._2.isDefined).map(_._2.get)
              toProcess.find(bp => !processNewBlock(bp, local = false)).foreach { case failedBlock =>
                log.warn(s"Can't apply block: ${failedBlock.json}")
                gotoSyncing()
              }
              gotoGettingBlock(witnesses, updBlocks.drop(toProcess.length))
            } else gotoGettingBlock(witnesses, updBlocks)
        }
    }: Receive)

  //accept only new block from local or remote
  def synced: Receive = state(HistorySynchronizer.Synced, {
    case block: Block =>
      processNewBlock(block, local = true)

    case ConsideredValue(Some(networkScore: History.BlockchainScore), witnesses) =>
      if (networkScore > history.score()) gotoGettingExtension(networkScore, witnesses)

    case DataFromPeer(msgId, block: Block@unchecked, _)
      if msgId == BlockMessageSpec.messageCode && block.cast[Block].isDefined =>
      processNewBlock(block, local = false)
  }: Receive)

  private def gotoSyncing(): Receive = {
    log.debug("Transition to syncing")
    context become syncing
    blockGenerator ! BlockGenerator.StopGeneration
    scoreObserver ! ScoreObserver.GetScore
    syncing
  }

  private def gotoGettingExtension(betterScore: BigInt, witnesses: Seq[ConnectedPeer]): Unit = {
    log.debug("Transition to gettingExtension")
    blockGenerator ! BlockGenerator.StopGeneration
    context.system.scheduler.scheduleOnce(GettingExtensionTimeout)(self ! GettingExtensionTimeout)
    context become gettingExtension(betterScore, witnesses)
  }

  private def gotoGettingBlock(witnesses: Seq[ConnectedPeer], blocks: Seq[(BlockId, Option[Block])]): Receive = {
    log.debug("Transition to gettingBlock")
    context.system.scheduler.scheduleOnce(GettingBlockTimeout)(self ! GettingBlockTimeout)
    context become gettingBlock(witnesses, blocks)
    gettingBlock(witnesses, blocks)
  }

  private def gotoSynced(): Receive = {
    log.debug("Transition to synced")
    blockGenerator ! BlockGenerator.StartGeneration
    context become synced
    synced
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
            s"""After appending block(local: $local, parent: ${Base58.encode(block.referenceField.value)}):
            (height, score) = ($oldHeight, $oldScore) vs (${history.height()}, ${history.score()})""")
          true
        case Failure(e) =>
          e.printStackTrace()
          log.warn(s"failed to append block: $e")
          false
      }
    } else {
      log.warn(s"Invalid new block(local: $local): ${block.json}")
      false
    }
  }.getOrElse(false)
}

object HistorySynchronizer {

  sealed trait Status {
    val name: String
  }

  case object Syncing extends Status {
    override val name = "syncing"
  }

  case object GettingExtension extends Status {
    override val name = "getting extension"
  }

  case object GettingBlock extends Status {
    override val name = "getting block"
  }

  case object Synced extends Status {
    override val name = "synced"
  }

  case class CheckBlock(id: BlockId)

  case object GetStatus

}