package scorex.network.redone

import akka.actor.FSM
import scorex.app.Application
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network._
import scorex.network.message.Message
import scorex.network.redone.NetworkObject.ConsideredValue
import scorex.transaction.{BlockChain, History}
import shapeless.Typeable._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


class HistorySynchronizer(application: Application)
  extends ViewSynchronizer with FSM[HistorySynchronizer.Status, Seq[ConnectedPeer]] {

  import HistorySynchronizer._

  implicit val consensusModule = application.consensusModule
  implicit val transactionalModule = application.transactionModule

  import application.basicMessagesSpecsRepo._

  override val messageSpecs = Seq(ScoreMessageSpec, GetSignaturesSpec, SignaturesSpec, BlockMessageSpec, GetBlockSpec)

  lazy val scoreSyncer = new ScoreNetworkObject(self)

  lazy val history = application.history

  lazy val networkControllerRef = application.networkController

  lazy val blockGenerator = application.blockGenerator

  override def preStart = {
    super.preStart()
    context.system.scheduler.schedule(1.second, 1.seconds) {
      val ntwMsg = Message(ScoreMessageSpec, Right(history.score()), None)
      val stn = NetworkController.SendToNetwork(ntwMsg, SendToRandom)
      networkControllerRef ! stn
    }
  }

  application.settings.offlineGeneration match {
    case true => startWith(Synced, Seq())
    case false => startWith(ScoreNotCompared, Seq())
  }

  when(ScoreNotCompared) {
    //init signal(boxed Unit) matching
    case Event(Unit, _) =>
      stay()

    case Event(ConsideredValue(Some(networkScore: History.BlockchainScore), witnesses), _) =>
      val localScore = history.score()
      if (networkScore > localScore) goto(Syncing) using witnesses
      else goto(Synced) using Seq()
  }

  when(Syncing){
    case Event(ConsideredValue(Some(networkScore: History.BlockchainScore), witnesses), _) =>
      val localScore = history.score()
      if (networkScore > localScore){
        log.info("networkScore > localScore")
        val msg = Message(GetSignaturesSpec, Right(history.lastSignatures(100)), None)
        networkControllerRef ! NetworkController.SendToNetwork(msg, SendToChosen(witnesses))
        goto(GettingExtension) using witnesses
      } else goto(Synced) using Seq()
  }

  private val blocksReceived = mutable.Map[BlockId, Option[Block]]()

  when(GettingExtension, 1.minute) {
    case Event(StateTimeout, _) =>
      goto(Syncing)

    //todo: aggregating function for block ids (like score has)
    //todo: check sender
    case Event(DataFromPeer(msgId, blockIds: Seq[Block.BlockId]@unchecked, remote), _)
      if msgId == SignaturesSpec.messageCode && blockIds.cast[Seq[Block.BlockId]].isDefined =>

      log.info(s"Got SignaturesMessage with ${blockIds.length} sigs")
      val common = blockIds.head
      assert(application.history.contains(common)) //todo: ?
      application.history.removeAfter(common)

      blocksReceived.clear()

      blockIds.tail.foreach { blockId =>
        blocksReceived += blockId -> None
        networkControllerRef ! NetworkController.SendToNetwork(Message(GetBlockSpec, Right(blockId), None), SendToChosen(Seq(remote)))
      }
      stay()

    case Event(DataFromPeer(msgId, block: Block@unchecked, remote), _)
      if msgId == BlockMessageSpec.messageCode && block.cast[Block].isDefined =>

      val blockId = block.uniqueId
      if(blocksReceived.contains(blockId)){
        blocksReceived.update(blockId, Some(block))
      }

      if(blocksReceived.forall(_._2.isDefined)){
        blocksReceived.values.foreach{bOpt=>
          processNewBlock(bOpt.get, local = false)
        }
        goto(Syncing)
      } else stay()
  }

  //accept only new block from local or remote
  when(Synced) {
    //init signal(boxed Unit) matching
    case Event(Unit, _) =>
      stay()

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
        val headers = application.history.asInstanceOf[BlockChain].getSignatures(parent, application.settings.MaxBlocksChunks)
        if (headers.nonEmpty) {
          val msg = Message(SignaturesSpec, Right(Seq(parent) ++ headers), None)
          val ss = SendToChosen(Seq(remote))
          networkControllerRef ! SendToNetwork(msg, ss)
          true
        } else false
      }
      stay()


    //todo: check sender
    case Event(DataFromPeer(msgId, sig: Block.BlockId@unchecked, remote), _)
      if msgId == GetBlockSpec.messageCode =>

      application.history.blockById(sig).foreach { b =>
        val msg = Message(BlockMessageSpec, Right(b), None)
        val ss = SendToChosen(Seq(remote))
        networkControllerRef ! SendToNetwork(msg, ss)
      }
      stay()

    case nonsense: Any =>
      log.warning(s"HistorySynchronizer: got something strange in the state ($stateName) :: $nonsense")
      stay()
  }

  onTransition {
    case _ -> Synced =>
      blockGenerator ! BlockGenerator.StartGeneration

    case Synced -> _ =>
      blockGenerator ! BlockGenerator.StopGeneration
  }

  initialize()

  def processNewBlock(block: Block, local:Boolean) = {
    if (block.isValid) {
      log.info(s"New block: $block local: $local")
      application.state.processBlock(block)
      history.appendBlock(block)

      block.transactionModule.clearFromUnconfirmed(block.transactionDataField.value)

      //broadcast block only if it is generated locally
      if (!local) {
        val blockMsg = Message(BlockMessageSpec, Right(block), None)
        networkControllerRef ! SendToNetwork(blockMsg, Broadcast)
      }
    } else {
      log.warning(s"Non-valid block: $block local: $local")
    }
  }
}

object HistorySynchronizer {

  sealed trait Status

  case object ScoreNotCompared extends Status

  case object Syncing extends Status

  case object GettingExtension extends Status

  case object Synced extends Status

}
