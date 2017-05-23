package scorex.network

import java.net.{InetAddress, InetSocketAddress, NetworkInterface, URI}

import akka.actor._
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import com.wavesplatform.settings.NetworkSettings
import scorex.network.message.{Message, MessageHandler, MessageSpec}
import scorex.network.peer.PeerManager
import scorex.network.peer.PeerManager.{CloseAllConnections, CloseAllConnectionsComplete}
import scorex.utils.ScorexLogging

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class NetworkController(networkSettings: NetworkSettings,
                        uPnP: => UPnP, peerManager: ActorRef, messagesHandler: MessageHandler)
  extends Actor with ScorexLogging {

  import NetworkController._

  lazy val localAddress = new InetSocketAddress(InetAddress.getByName(networkSettings.bindAddress), networkSettings.port)

  lazy val ownSocketAddress = getDeclaredHost.flatMap(host => Try(InetAddress.getByName(host)).toOption)
    .orElse {
      if (networkSettings.uPnPSettings.enable) uPnP.externalAddress else None
    }.map(inetAddress => new InetSocketAddress(inetAddress, getDeclaredPort.getOrElse(networkSettings.port)))

  lazy val connTimeout = Some(networkSettings.connectionTimeout)

  // there is not recovery for broken connections
  override val supervisorStrategy = SupervisorStrategy.stoppingStrategy

  private implicit val system = context.system

  private val messageHandlers = mutable.Map[Seq[Message.MessageCode], ActorRef]()

  private var listeningStopped: Boolean = false
  private var connectionClosed: Boolean = false
  private var maybeRequester: Option[ActorRef] = None

  //check own declared address for validity
  if (!networkSettings.localOnly) {
    getDeclaredHost.forall { myHost =>
      Try {
        val myAddress = InetAddress.getAllByName(myHost)

        NetworkInterface.getNetworkInterfaces.asScala.exists { networkInterface =>
          networkInterface.getInterfaceAddresses.asScala.exists { interfaceAddress =>
            val externalAddress = interfaceAddress.getAddress
            myAddress.contains(externalAddress)
          }
        } match {
          case true => true
          case false =>
            if (networkSettings.uPnPSettings.enable) {
              val externalAddress = uPnP.externalAddress
              myAddress.contains(externalAddress)
            } else false
        }
      }.recover { case t: Throwable =>
        log.error("Declared address validation failed: ", t)
        false
      }.getOrElse(false)
    }
  }

  private val listener = context.actorOf(Props(classOf[NetworkListener], self, peerManager, localAddress),
    "network-listener")

  override def postRestart(thr: Throwable): Unit = {
    log.warn(s"restart because of $thr.getMessage")
    context stop self
  }

  log.info(s"Declared address: $ownSocketAddress")

  def businessLogic: Receive = {
    //a message coming in from another peer
    case Message(spec, Left(msgBytes), Some(remote)) =>
      val msgId = spec.messageCode

      spec.deserializeData(msgBytes) match {
        case Success(content) =>
          messageHandlers.find(_._1.contains(msgId)).map(_._2) match {
            case Some(handler) =>
              handler ! DataFromPeer(msgId, content, remote)

            case None =>
              log.error("No handlers found for message: " + msgId)
            //todo: ban a peer
          }
        case Failure(e) =>
          log.error("Failed to deserialize data: " + e.getMessage)
        //todo: ban peer
      }

    case stn@SendToNetwork(_, _) =>
      val delay = 0
      system.scheduler.scheduleOnce(delay.millis) {
        peerManager ! stn
      }
  }

  def peerLogic: Receive = {
    case ConnectTo(remote) =>
      log.info(s"Connecting to: $remote")
      IO(Tcp) ! Connect(remote, localAddress = None, timeout = connTimeout)

    case Connected(remote, _) =>
      val connection = sender()
      createPeerHandler(connection, remote)

    case CommandFailed(c: Connect) =>
      log.info("Failed to connect to : " + c.remoteAddress)
      peerManager ! PeerManager.Disconnected(c.remoteAddress)
  }

  //calls from API / application
  def interfaceCalls: Receive = {
    case ShutdownNetwork =>
      maybeRequester = Some(sender())
      log.info("Going to shutdown all connections & unbind port")
      peerManager ! CloseAllConnections
      listener ! NetworkListener.StopListen

    case CloseAllConnectionsComplete =>
      connectionClosed = true
      sendShutdownComplete()

    case ListeningStopped =>
      listeningStopped = true
      sendShutdownComplete()
  }

  private def sendShutdownComplete(): Unit = {
    if (connectionClosed && listeningStopped) maybeRequester.foreach(_ ! NetworkShutdownComplete)
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse interfaceCalls orElse {
    case RegisterMessagesHandler(specs, handler) =>
      messageHandlers += specs.map(_.messageCode) -> handler

    case CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd)

    case nonsense: Any =>
      log.warn(s"NetworkController: got something strange $nonsense")
  }

  private def getDeclaredUri: Option[URI] = Try(new URI(s"http://${networkSettings.declaredAddress}")).toOption

  private def getDeclaredHost: Option[String] = getDeclaredUri.map(_.getHost)

  private def getDeclaredPort: Option[Int] = getDeclaredUri.map(_.getPort).filterNot(_ == -1)

  private def bindingLogic: Receive = {
    case ReadyToListen =>
      listener ! NetworkListener.StartListen

    case ListeningStarted =>
      log.info("Successfully started listening")
      context.system.scheduler.schedule(600.millis, 5.seconds)(peerManager ! PeerManager.CheckPeers)
      log.debug("Outbound connections scheduler started")

    case ListeningFailed =>
      log.error("Failed to start listening!")
      throw new IllegalStateException("Failed to start listening!")

    case InboundConnection(connection, remote) =>
      createPeerHandler(connection, remote)
  }

  private def createPeerHandler(connection: ActorRef, remote: InetSocketAddress): Unit = {
    val handler = context.actorOf(Props(new PeerConnectionHandler(peerManager,
      connection,
      remote,
      messagesHandler,
      networkSettings)))
    peerManager ! PeerManager.Connected(remote, handler, ownSocketAddress)
  }
}

object NetworkController {

  case class RegisterMessagesHandler(specs: Seq[MessageSpec[_]], handler: ActorRef)

  case class DataFromPeer[V](messageType: Message.MessageCode, data: V, source: ConnectedPeer)

  case class SendToNetwork(message: Message[_], sendingStrategy: SendingStrategy)

  case class ConnectTo(address: InetSocketAddress)

  case class InboundConnection(connection: ActorRef, remote: InetSocketAddress)

  case object ShutdownNetwork

  case object NetworkShutdownComplete

  case object ListeningStarted

  case object ListeningStopped

  case object ListeningFailed

  case object ReadyToListen

}
