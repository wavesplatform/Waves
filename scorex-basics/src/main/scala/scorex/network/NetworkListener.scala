package scorex.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import scorex.utils.ScorexLogging

class NetworkListener(networkController: ActorRef, bindAddress: InetSocketAddress) extends Actor with ScorexLogging {

  private implicit val system = context.system

  override def receive: Receive = {
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
      log.debug(s"Inbound connection from $remote")
      val connection = sender()
      networkController ! NetworkController.InboundConnection(connection, remote)
  }
}

object NetworkListener {

  case object StartListen

  case object StopListen

}