package scorex.network.redone

import java.net.InetSocketAddress

import scorex.app.Application
import scorex.network.NetworkController.DataFromPeer
import scorex.network.message.Message

import scala.concurrent.duration._


class PeerSynchronizer(application:Application) extends ViewSynchronizer {

  import application.basicMessagesSpecsRepo._
  override val messageSpecs = Seq(GetPeersSpec, PeersSpec)

  override val networkControllerRef = application.networkController

  private val peerManager = application.peerManager

  override def preStart = {
    super.preStart()

    val ntwMsg = Message(GetPeersSpec, Right(Unit), None)
    context.system.scheduler.schedule(2.seconds, 1.second)(networkControllerRef ! ntwMsg)
  }

  override def receive = {
    case DataFromPeer(peers: Seq[InetSocketAddress], remote) =>
      peers.foreach(peerManager.addPeer)

    case DataFromPeer(Unit, remote) =>
      val peers = peerManager.knownPeers().take(3) // make configurable, check on receiving
      networkControllerRef ! Message(PeersSpec, Right(peers), None)
  }
}
