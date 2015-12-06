package scorex.network.redone

import java.net.InetSocketAddress

import scorex.app.Application
import scorex.network.{SendToChosen, SendToRandom, NetworkController}
import scorex.network.NetworkController.DataFromPeer
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
    context.system.scheduler.schedule(2.seconds, 1.second)(networkControllerRef ! stn)
  }

  //todo: write tests
  override def receive = {
    case DataFromPeer(peers: Seq[InetSocketAddress] @unchecked, remote)
      if peers.cast[Seq[InetSocketAddress]].isDefined  =>
      peers.foreach(peerManager.addPeer)

    //boxed Unit match
    case DataFromPeer((), remote) =>
      val peers = peerManager.knownPeers().take(3) //todo: make configurable, check on receiving
      val msg = Message(PeersSpec, Right(peers), None)
      networkControllerRef ! SendToChosen(Seq(remote))

    case nonsense: Any => log.warn(s"PeerSynchronizer: got something strange $nonsense")
  }
}
