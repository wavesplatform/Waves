package scorex.network

import scorex.app.Application
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.crypto.encode.Base58
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.NetworkObject.ConsideredValue
import scorex.network.message.Message
import scorex.transaction.History
import scorex.utils.ScorexLogging
import shapeless.Typeable._

import scala.collection.mutable
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

  private lazy val scoreSyncer = new ScoreNetworkObject(self)

  private lazy val history = application.history

  protected override lazy val networkControllerRef = application.networkController

  private lazy val blockGenerator = application.blockGenerator

  private val GettingExtensionTimeout = 40.seconds
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

  def syncing: Receive = ({
    case ConsideredValue(Some(networkScore: History.BlockchainScore), witnesses) =>
      val localScore = history.score()
      if (networkScore > localScore) {
        log.info(s"networkScore=$networkScore > localScore=$localScore")
        val lastIds = history.lastBlocks(100).map(_.uniqueId)
        val msg = Message(GetSignaturesSpec, Right(lastIds), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(witnesses))
        gotoGettingExtension(witnesses)
      } else gotoSynced()
  }: Receive) orElse commonLogic

  private val blocksToReceive = mutable.Queue[BlockId]()

  def gettingExtension(witnesses: Seq[ConnectedPeer]): Receive = ({
    case GettingExtensionTimeout => gotoSyncing()

    //todo: aggregating function for block ids (like score has)
    case DataFromPeer(msgId, blockIds: Seq[Block.BlockId]@unchecked, remote)
      if msgId == SignaturesSpec.messageCode &&
        blockIds.cast[Seq[Block.BlockId]].isDefined &&
        witnesses.contains(remote) => //todo: ban if non-expected sender

      val newBLockIds = blockIds.filter(!history.contains(_))
      log.info(s"Got SignaturesMessage with ${blockIds.length} sigs, ${newBLockIds.size} are new")

      val common = blockIds.head
      assert(application.history.contains(common)) //todo: what if not?
      Try(application.blockStorage.removeAfter(common)) //todo we don't need this call for blockTree

      blocksToReceive.clear()

      val newContext = if (newBLockIds.nonEmpty) {
        newBLockIds.foreach { blockId =>
          blocksToReceive += blockId
        }

        networkControllerRef ! NetworkController.SendToNetwork(Message(GetBlockSpec, Right(blocksToReceive.front), None),
          SendToChosen(Seq(remote)))

        gotoGettingBlock(witnesses)
      } else syncing

      context become newContext
  }: Receive) orElse commonLogic

  def gettingBlock(witnesses: Seq[ConnectedPeer]): Receive = ({
    case GettingBlockTimeout => //15.seconds
      blocksToReceive.clear()
      gotoSyncing()

    case CheckBlock(blockId) =>
      if (blocksToReceive.nonEmpty && blocksToReceive.front.sameElements(blockId)) {
        val sendTo = SendToRandomFromChosen(witnesses)
        val stn = NetworkController.SendToNetwork(Message(GetBlockSpec, Right(blockId), None), sendTo)
        networkControllerRef ! stn
      }

    case DataFromPeer(msgId, block: Block@unchecked, remote)
      if msgId == BlockMessageSpec.messageCode && block.cast[Block].isDefined
        && blocksToReceive.front.sameElements(block.uniqueId) =>

      val blockId = block.uniqueId
      log.info("Got block: " + Base58.encode(blockId))

      if (processNewBlock(block, local = false)) {
        if (blocksToReceive.nonEmpty && blocksToReceive.front.sameElements(blockId)) blocksToReceive.dequeue()

        if (blocksToReceive.nonEmpty) {
          val blockId = blocksToReceive.front
          val ss = SendToRandomFromChosen(Seq(remote))
          val stn = NetworkController.SendToNetwork(Message(GetBlockSpec, Right(blockId), None), ss)
          networkControllerRef ! stn
          context.system.scheduler.scheduleOnce(5.seconds)(self ! CheckBlock(blockId))
        }
      } else if (!history.contains(block.referenceField.value)) {
        log.warn("No parent block in history")
        //blocksToReceive.clear()
        //blocksToReceive.enqueue(block.referenceField.value)
      }

      if (blocksToReceive.nonEmpty) {
        self ! CheckBlock(blocksToReceive.front)
      } else {
        context become syncing
      }
  }: Receive) orElse commonLogic

  //accept only new block from local or remote
  def synced: Receive = ({
    case block: Block =>
      processNewBlock(block, local = true)

    case ConsideredValue(Some(networkScore: History.BlockchainScore), witnesses) =>
      val localScore = history.score()
      if (networkScore > localScore) {
        blockGenerator ! BlockGenerator.StopGeneration
        gotoGettingExtension(witnesses)
      }

    case DataFromPeer(msgId, block: Block@unchecked, remote)
      if msgId == BlockMessageSpec.messageCode && block.cast[Block].isDefined =>
      processNewBlock(block, local = false)
  }: Receive) orElse commonLogic

  //common logic for all the states
  def commonLogic: Receive = {
    //todo: check sender
    case DataFromPeer(msgId, content: History.BlockchainScore, remote)
      if msgId == ScoreMessageSpec.messageCode =>

      scoreSyncer.networkUpdate(remote, content)

    case ConsideredValue(Some(networkScore: History.BlockchainScore), witnesses) =>
      log.info("Got unhandled ConsideredValue(score)")

    //the signals to initialize
    case Unit =>

    //timeout signals
    case f: FiniteDuration =>

    case nonsense: Any =>
      log.warn(s"Got something strange: $nonsense")
  }

  private def gotoSyncing() = {
    log.debug("Transition to syncing")
    scoreSyncer.consideredValue.foreach(cv => self ! cv)
    context become syncing
    syncing
  }

  private def gotoGettingExtension(witnesses: Seq[ConnectedPeer]): Unit = {
    log.debug("Transition to gettingExtension")
    context.system.scheduler.scheduleOnce(GettingExtensionTimeout)(self ! GettingExtensionTimeout)
    context become gettingExtension(witnesses)
  }

  private def gotoGettingBlock(witnesses: Seq[ConnectedPeer]): Receive = {
    log.debug("Transition to gettingBlock")
    context.system.scheduler.scheduleOnce(GettingBlockTimeout)(self ! GettingBlockTimeout)
    gettingBlock(witnesses)
  }

  private def gotoSynced() = {
    log.debug("Transition to synced")
    blockGenerator ! BlockGenerator.StartGeneration
    context become synced
    synced
  }

  private def processNewBlock(block: Block, local: Boolean): Boolean = {
    if (block.isValid) {
      log.info(s"New block(local: $local): ${block.json}")

      if (local) networkControllerRef ! SendToNetwork(Message(BlockMessageSpec, Right(block), None), Broadcast)
      if (!local || application.settings.offlineGeneration) {
        val oldHeight = history.height()
        val oldScore = history.score()
        val appending = transactionalModule.blockStorage.appendBlock(block)

        appending match {
          case Success(_) =>
            block.transactionModule.clearFromUnconfirmed(block.transactionDataField.value)
            log.info(s"(height, score) = ($oldHeight, $oldScore) vs (${history.height()}, ${history.score()})")
          case Failure(e) =>
            e.printStackTrace()
            log.warn(s"failed to append block: $e")
        }
        appending.isSuccess
      } else true
    } else {
      log.warn(s"Invalid new block(local: $local): ${block.json}")
      false
    }
  }
}

object HistorySynchronizer {

  sealed trait Status

  case object Syncing extends Status

  case object GettingExtension extends Status

  case object GettingBlock extends Status

  case object Synced extends Status

  case class CheckBlock(id: BlockId)

}