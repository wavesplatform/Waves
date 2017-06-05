package scorex.network

import akka.actor.ActorRef
import com.wavesplatform.state2.ByteStr
import scorex.block.Block
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.message.Message
import scorex.utils.ScorexLogging
import scorex.network.message._
import scorex.transaction.History

class HistoryReplier(protected val networkControllerRef: ActorRef, history: History, maxChainLength: Int) extends ViewSynchronizer with ScorexLogging {

  override val messageSpecs = Seq(GetSignaturesSpec, GetBlockSpec)

  override def receive: Receive = {

    case DataFromPeer(msgId, otherSigs: Seq[Array[Byte]]@unchecked, remote)
      if msgId == GetSignaturesSpec.messageCode =>

      log.info(s"Got GetSignaturesMessage with ${otherSigs.length} sigs within")

      otherSigs.exists { p =>
        val parent = ByteStr(p)
        val headers = history.blockIdsAfter(parent, maxChainLength)

        if (headers.nonEmpty) {
          val msg = Message(SignaturesSpec, Right((Seq(parent) ++ headers).map(_.arr)), None)
          val ss = SendToChosen(remote)
          networkControllerRef ! SendToNetwork(msg, ss)
          true
        } else false
      }

    case DataFromPeer(msgId, sig: Array[Byte], remote)
      if msgId == GetBlockSpec.messageCode =>

      history.heightOf(ByteStr(sig)).flatMap(history.blockBytes).foreach { b =>
        val msg = Message(BlockMessageSpec, Left(b), None)
        val ss = SendToChosen(remote)
        networkControllerRef ! SendToNetwork(msg, ss)
      }
  }
}
