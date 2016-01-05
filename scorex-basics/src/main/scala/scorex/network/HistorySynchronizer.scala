package scorex.network

import akka.actor.FSM
import scorex.app.Application
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.NetworkObject.ConsideredValue
import scorex.network.message.Message
import scorex.transaction.History
import shapeless.Typeable._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Try

//todo: write tests
class HistorySynchronizer(application: Application)
  extends ViewSynchronizer with FSM[HistorySynchronizer.Status, Seq[ConnectedPeer]] {

  import HistorySynchronizer._
  import application.basicMessagesSpecsRepo._

  private implicit val consensusModule = application.consensusModule
  private implicit val transactionalModule = application.transactionModule

  override val messageSpecs = Seq(ScoreMessageSpec, GetSignaturesSpec, SignaturesSpec, BlockMessageSpec, GetBlockSpec)

  private lazy val scoreSyncer = new ScoreNetworkObject(self)

  private lazy val history = application.history

  protected override lazy val networkControllerRef = application.networkController

  private lazy val blockGenerator = application.blockGenerator

  override def preStart: Unit = {
    super.preStart()
    context.system.scheduler.schedule(1.second, 1.seconds) {
      val msg = Message(ScoreMessageSpec, Right(history.score()), None)
      networkControllerRef ! NetworkController.SendToNetwork(msg, SendToRandom)
    }
  }

  val initialState = if (application.settings.offlineGeneration) Synced else Syncing
  startWith(initialState, Seq())

  when(Syncing) {
    case Event(ConsideredValue(Some(networkScore: History.BlockchainScore), witnesses), _) =>
      val localScore = history.score()
      if (networkScore > localScore) {
        log.info("networkScore > localScore")
        val lastIds = history.lastBlocks(100).map(_.uniqueId)
        val msg = Message(GetSignaturesSpec, Right(lastIds), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(witnesses))
        goto(GettingExtension) using witnesses
      } else goto(Synced) using Seq()
  }

  private val blocksToReceive = mutable.Queue[BlockId]()

  when(GettingExtension, 15.seconds) {
    case Event(StateTimeout, _) =>
      goto(Syncing)

    //todo: aggregating function for block ids (like score has)
    case Event(DataFromPeer(msgId, blockIds: Seq[Block.BlockId]@unchecked, remote), witnesses)
      if msgId == SignaturesSpec.messageCode &&
        blockIds.cast[Seq[Block.BlockId]].isDefined &&
        witnesses.contains(remote) => //todo: ban if non-expected sender

      log.info(s"Got SignaturesMessage with ${blockIds.length} sigs")
      val common = blockIds.head
      assert(application.history.contains(common)) //todo: what if not?
      Try(application.blockStorage.removeAfter(common)) //todo we don't need this call for blockTree

      blocksToReceive.clear()

      blockIds.tail.foreach { blockId =>
        blocksToReceive += blockId
      }

      val firstReq = blockIds.tail.head
      networkControllerRef ! NetworkController.SendToNetwork(Message(GetBlockSpec, Right(firstReq), None), SendToChosen(Seq(remote)))
      goto(GettingBlock)
  }

  when(GettingBlock) {
    case Event(CheckBlock(blockId), witnesses) =>
      if (blocksToReceive.front.sameElements(blockId)) {
        val sendTo = SendToRandomFromChosen(witnesses)
        val stn = NetworkController.SendToNetwork(Message(GetBlockSpec, Right(blockId), None), sendTo)
        networkControllerRef ! stn
      }
      stay()

    case Event(DataFromPeer(msgId, block: Block@unchecked, remote), _)
      if msgId == BlockMessageSpec.messageCode && block.cast[Block].isDefined =>

      require(blocksToReceive.nonEmpty)

      val blockId = block.uniqueId
      log.info("Got block: " + blockId)

      if (blocksToReceive.front.sameElements(blockId)) {
        processNewBlock(block, local = false)
        blocksToReceive.dequeue()

        if (blocksToReceive.isEmpty) {
          goto(Syncing) using Seq()
        } else {
          val blockId = blocksToReceive.front
          val ss = SendToRandomFromChosen(Seq(remote))
          val stn = NetworkController.SendToNetwork(Message(GetBlockSpec, Right(blockId), None), ss)
          networkControllerRef ! stn
          context.system.scheduler.scheduleOnce(5.seconds)(self ! CheckBlock(blockId))
          stay()
        }
      } else {
        self ! CheckBlock(blockId)
        stay()
      }
  }

  //accept only new block from local or remote
  when(Synced) {
    case Event(DataFromPeer(msgId, block: Block@unchecked, remote), _)
      if msgId == BlockMessageSpec.messageCode && block.cast[Block].isDefined =>
      processNewBlock(block, local = false)
      stay()

    case Event(block: Block, _) =>
      processNewBlock(block, local = true)
      stay()

    case Event(ConsideredValue(Some(networkScore: History.BlockchainScore), witnesses), _) =>
      val localScore = history.score()
      if (networkScore > localScore) goto(Syncing) using witnesses
      else stay() using Seq()
  }

  //common logic for all the states
  whenUnhandled {
    //init signal(boxed Unit) matching
    case Event(Unit, _) if stateName == initialState =>
      stay()

    //todo: check sender
    case Event(DataFromPeer(msgId, content: History.BlockchainScore, remote), _)
      if msgId == ScoreMessageSpec.messageCode =>
      scoreSyncer.networkUpdate(remote, content)
      stay()

    //todo: check sender
    case Event(DataFromPeer(msgId, otherSigs: Seq[Block.BlockId]@unchecked, remote), _)
      if msgId == GetSignaturesSpec.messageCode && otherSigs.cast[Seq[Block.BlockId]].isDefined =>

      log.info(s"Got GetSignaturesMessage with ${otherSigs.length} sigs within")

      otherSigs.exists { parent =>
        val headers = application.history.lookForward(parent, application.settings.MaxBlocksChunks)

        if (headers.nonEmpty) {
          val msg = Message(SignaturesSpec, Right(Seq(parent) ++ headers), None)
          val ss = SendToChosen(Seq(remote))
          networkControllerRef ! SendToNetwork(msg, ss)
          true
        } else false
      }
      stay()


    //todo: check sender?
    case Event(DataFromPeer(msgId, sig: Block.BlockId@unchecked, remote), _)
      if msgId == GetBlockSpec.messageCode =>

      application.history.blockById(sig).foreach { b =>
        val msg = Message(BlockMessageSpec, Right(b), None)
        val ss = SendToChosen(Seq(remote))
        networkControllerRef ! SendToNetwork(msg, ss)
      }
      stay()

    case Event(ConsideredValue(Some(networkScore: History.BlockchainScore), witnesses), _) =>
      stay()

    case nonsense: Any =>
      log.warning(s"HistorySynchronizer: got something strange in the state ($stateName) :: $nonsense")
      stay()
  }

  onTransition {
    case _ -> Syncing =>
      scoreSyncer.consideredValue.foreach { cv =>
        self ! cv
      }

    case _ -> Synced =>
      blockGenerator ! BlockGenerator.StartGeneration

    case Synced -> _ =>
      blockGenerator ! BlockGenerator.StopGeneration
  }

  initialize()

  private def processNewBlock(block: Block, local: Boolean) = {
    if (block.isValid) {
      log.info(s"New block(local: $local): ${block.json}")
      transactionalModule.blockStorage.appendBlock(block)

      block.transactionModule.clearFromUnconfirmed(block.transactionDataField.value)

      //broadcast block only if it is generated locally
      if (local) {
        val blockMsg = Message(BlockMessageSpec, Right(block), None)
        networkControllerRef ! SendToNetwork(blockMsg, Broadcast)
      }
    } else log.warning(s"Non-valid block: ${block.json} local: $local")
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
