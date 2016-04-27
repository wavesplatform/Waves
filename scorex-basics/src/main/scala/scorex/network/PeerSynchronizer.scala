package scorex.network

import java.net.InetSocketAddress

import akka.pattern.ask
import akka.util.Timeout
import scorex.app.Application
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.message.Message
import scorex.network.peer.PeerManager
import scorex.network.peer.PeerManager.RandomPeers
import scorex.utils.ScorexLogging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


class PeerSynchronizer(application: Application) extends ViewSynchronizer with ScorexLogging {

  import application.basicMessagesSpecsRepo._

  private implicit val timeout = Timeout(5.seconds)

  override val messageSpecs = Seq(GetPeersSpec, PeersSpec)
  override val networkControllerRef = application.networkController

  private val peerManager = application.peerManager

  override def preStart: Unit = {
    super.preStart()

    val stn = NetworkController.SendToNetwork(Message(GetPeersSpec, Right(), None), SendToRandom)
    context.system.scheduler.schedule(2.seconds, 10.seconds)(networkControllerRef ! stn)
  }

  override def receive: Receive = {
    case DataFromPeer(msgId, peers: Seq[InetSocketAddress]@unchecked, remote)
      if msgId == PeersSpec.messageCode && peers.cast[Seq[InetSocketAddress]].isDefined =>

      peers.foreach(isa => peerManager ! PeerManager.AddOrUpdatePeer(isa, None, None))

    case DataFromPeer(msgId, _, remote) if msgId == GetPeersSpec.messageCode =>

      //todo: externalize the number, check on receiving
      (peerManager ? RandomPeers(3))
        .mapTo[Seq[InetSocketAddress]]
        .foreach { peers =>
          val msg = Message(PeersSpec, Right(peers), None)
          networkControllerRef ! SendToNetwork(msg, SendToChosen(Seq(remote)))
        }

    case nonsense: Any => log.warn(s"PeerSynchronizer: got something strange $nonsense")
  }
}