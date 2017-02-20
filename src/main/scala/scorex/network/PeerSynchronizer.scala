package scorex.network

import java.net.InetSocketAddress

import akka.actor.Scheduler
import akka.pattern.ask
import akka.util.Timeout
import scorex.app.Application
import scorex.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.network.PeerSynchronizer.RequestDataFromPeer
import scorex.network.message.Message
import scorex.network.peer.PeerManager
import scorex.network.peer.PeerManager.GetRandomPeersToBroadcast
import scorex.utils.ScorexLogging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


class PeerSynchronizer(application: Application) extends ViewSynchronizer with ScorexLogging {

  private val basicMessagesSpecsRepo = application.basicMessagesSpecsRepo
  import basicMessagesSpecsRepo._

  protected lazy override val networkControllerRef = application.networkController
  override val messageSpecs = Seq(GetPeersSpec, PeersSpec)
  private implicit val timeout = Timeout(5.seconds)
  private val maxPeersToBroadcast = 3
  private val peerManager = application.peerManager
  private val peersDataBroadcastDelay = application.settings.networkSettings.peersBroadcastInterval
  private val stn = NetworkController.SendToNetwork(Message(GetPeersSpec, Right(()), None), SendToRandom)
  private var hasRequested = false
  private var unrequestedPacketsCount = 0

  protected def scheduler: Scheduler = context.system.scheduler

  scheduler.schedule(peersDataBroadcastDelay, peersDataBroadcastDelay)(self ! RequestDataFromPeer)

  override def receive: Receive = {
    case RequestDataFromPeer =>
      hasRequested = true
      networkControllerRef ! stn

    case DataFromPeer(msgId, peers: Seq[InetSocketAddress]@unchecked, remote)
      if hasRequested && msgId == PeersSpec.messageCode && peers.cast[Seq[InetSocketAddress]].isDefined =>
      hasRequested = false

      if (peers.size <= maxPeersToBroadcast) {
        peers
          .filter(_.getPort < application.settings.networkSettings.minEphemeralPortNumber)
          .foreach(isa => peerManager ! PeerManager.AddPeer(isa))
      }

    case DataFromPeer(msgId, _, remote)
      if !hasRequested && msgId == PeersSpec.messageCode =>
      unrequestedPacketsCount = unrequestedPacketsCount + 1
      if (unrequestedPacketsCount >= application.settings.networkSettings.unrequestedPacketsThreshold) {
        log.warn(s"Received too many data without requesting it from $remote")
        remote.blacklist()
      }

    case DataFromPeer(msgId, _, remote) if msgId == GetPeersSpec.messageCode =>
      (peerManager ? GetRandomPeersToBroadcast(maxPeersToBroadcast))
        .mapTo[Seq[InetSocketAddress]]
        .filter(_.nonEmpty)
        .foreach { peers =>
          val msg = Message(PeersSpec, Right(peers), None)
          networkControllerRef ! SendToNetwork(msg, SendToChosen(remote))
        }

    case nonsense: Any => log.warn(s"PeerSynchronizer: got something strange $nonsense")
  }
}

object PeerSynchronizer {

  case object RequestDataFromPeer

}
