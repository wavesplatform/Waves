package scorex.network

import scorex.app.Application
import scorex.block.Block
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.message.Message
import scorex.utils.ScorexLogging

class HistoryReplier(application: Application) extends ViewSynchronizer with ScorexLogging {

  import application.basicMessagesSpecsRepo._

  override val messageSpecs = Seq(GetSignaturesSpec, GetBlockSpec)
  protected override lazy val networkControllerRef = application.networkController

  override def receive: Receive = {

    //todo: check sender and otherSigs type
    case DataFromPeer(msgId, otherSigs: Seq[Block.BlockId]@unchecked, remote)
      if msgId == GetSignaturesSpec.messageCode =>

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

    //todo: check sender?
    case DataFromPeer(msgId, sig: Block.BlockId@unchecked, remote)
      if msgId == GetBlockSpec.messageCode =>

      application.history.blockById(sig).foreach { b =>
        val msg = Message(BlockMessageSpec, Right(b), None)
        val ss = SendToChosen(Seq(remote))
        networkControllerRef ! SendToNetwork(msg, ss)
      }

    //the signal to initialize
    case Unit =>
  }
}
