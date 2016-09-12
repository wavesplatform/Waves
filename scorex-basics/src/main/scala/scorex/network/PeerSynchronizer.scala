package scorex.network

import java.net.InetSocketAddress

import akka.pattern.ask
import akka.util.Timeout
import scorex.app.RunnableApplication
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.message.Message
import scorex.network.peer.PeerManager
import scorex.network.peer.PeerManager.GetRandomPeersToBroadcast
import scorex.utils.ScorexLogging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


class PeerSynchronizer(application: RunnableApplication) extends ViewSynchronizer with ScorexLogging {

  import application.basicMessagesSpecsRepo._

  private implicit val timeout = Timeout(5.seconds)

  override val messageSpecs = Seq(GetPeersSpec, PeersSpec)
  protected lazy override val networkControllerRef = application.networkController

  private val peerManager = application.peerManager

  private val peersDataBroadcastDelay = application.settings.peersDataBroadcastDelay
  private val stn = NetworkController.SendToNetwork(Message(GetPeersSpec, Right(), None), SendToRandom)
  context.system.scheduler.schedule(peersDataBroadcastDelay, peersDataBroadcastDelay)(networkControllerRef ! stn)

  override def receive: Receive = {
    case DataFromPeer(msgId, peers: Seq[InetSocketAddress]@unchecked, remote)
      if msgId == PeersSpec.messageCode && peers.cast[Seq[InetSocketAddress]].isDefined =>

      peers
        .filter(_.getPort < application.settings.minEphemeralPortNumber)
        .foreach(isa => peerManager ! PeerManager.AddOrUpdatePeer(isa, None, None))

    case DataFromPeer(msgId, _, remote) if msgId == GetPeersSpec.messageCode =>

      //todo: externalize the number, check on receiving
      (peerManager ? GetRandomPeersToBroadcast(3))
        .mapTo[Seq[InetSocketAddress]]
        .filter(_.nonEmpty)
        .foreach { peers =>
          val msg = Message(PeersSpec, Right(peers), None)
          networkControllerRef ! SendToNetwork(msg, SendToChosen(remote))
        }

    case nonsense: Any => log.warn(s"PeerSynchronizer: got something strange $nonsense")
  }
}