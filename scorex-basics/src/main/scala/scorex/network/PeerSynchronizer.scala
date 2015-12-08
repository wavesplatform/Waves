package scorex.network

import java.net.InetSocketAddress

import scorex.app.Application
import scorex.network.NetworkController.{SendToNetwork, DataFromPeer}
import scorex.network.message.Message
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import shapeless.Typeable._

//todo: avoid connecting to self
class PeerSynchronizer(application:Application) extends ViewSynchronizer with ScorexLogging {

  import application.basicMessagesSpecsRepo._
  override val messageSpecs = Seq(GetPeersSpec, PeersSpec)

  override val networkControllerRef = application.networkController

  private val peerManager = application.peerManager

  override def preStart = {
    super.preStart()

    val ntwMsg = Message(GetPeersSpec, Right(), None)
    val stn = NetworkController.SendToNetwork(ntwMsg, SendToRandom)
    context.system.scheduler.schedule(2.seconds, 5.seconds)(networkControllerRef ! stn)
  }

  //todo: write tests
  override def receive = {
    case DataFromPeer(msgId, peers: Seq[InetSocketAddress] @unchecked, remote)
      if msgId == PeersSpec.messageCode && peers.cast[Seq[InetSocketAddress]].isDefined  =>
      peers.foreach(peerManager.addPeer)

    case DataFromPeer(msgId, _, remote) if msgId == GetPeersSpec.messageCode =>
      val peers = peerManager.knownPeers().take(3) //todo: make configurable, check on receiving
      val msg = Message(PeersSpec, Right(peers), None)
      networkControllerRef ! SendToNetwork(msg, SendToChosen(Seq(remote)))

    case nonsense: Any => log.warn(s"PeerSynchronizer: got something strange $nonsense")
  }
}
