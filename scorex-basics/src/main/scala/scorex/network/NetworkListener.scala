package scorex.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import scorex.network.peer.PeerManager
import scorex.utils.ScorexLogging

import scala.collection.mutable

class NetworkListener(networkController: ActorRef, peerManager: ActorRef, bindAddress: InetSocketAddress)
  extends Actor with ScorexLogging {

  private implicit val system = context.system

  private val blocked: mutable.Set[String] = scala.collection.mutable.Set.empty[String]

  override def receive: Receive = initializing

  override def preStart(): Unit = {
    peerManager ! PeerManager.RegisterBlacklistListener(self)
  }

  override def postStop(): Unit = {
    peerManager ! PeerManager.UnregisterBlacklistListener(self)
  }

  private def initializing: Receive = {
    case PeerManager.ExistedBlacklist(hosts) =>
      log.info("Successfully registered to blacklist updates notifications")
      blocked ++= hosts
      networkController ! NetworkController.ReadyToListen

      context become working
  }

  private def working: Receive = {
    case PeerManager.BlackListUpdated(host) =>
      log.info(s"Blocking host: $host")
      blocked += host

    case NetworkListener.StartListen =>
      IO(Tcp) ! Bind(self, bindAddress)

    case NetworkListener.StopListen =>
      IO(Tcp) ! Unbind

    case Bound(localAddress) =>
      log.debug("Successfully bound to the port " + localAddress.getPort)
      networkController ! NetworkController.ListeningStarted

    case CommandFailed(_: Bind) =>
      log.error("Network port " + bindAddress.getPort + " already in use!")
      networkController ! NetworkController.ListeningFailed

    case Connected(remote, local) =>
      if (blocked.contains(remote.getAddress.getHostName)) {
        sender() ! Close
      } else {
        log.debug(s"Inbound connection from $remote")
        val connection = sender()
        networkController ! NetworkController.InboundConnection(connection, remote)
      }
  }
}

object NetworkListener {

  case object StartListen

  case object StopListen

  case class UpdateBlacklist(host: String)

}