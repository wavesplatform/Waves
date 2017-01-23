package scorex.network

import java.net.{InetAddress, InetSocketAddress, NetworkInterface, URI}

import akka.actor._
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.pattern._
import akka.util.Timeout
import scorex.app.RunnableApplication
import scorex.network.message.{Message, MessageSpec}
import scorex.network.peer.PeerManager
import scorex.network.peer.PeerManager.{CloseAllConnections, CloseAllConnectionsComplete}
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

/**
  * Control all network interaction
  * must be singleton
  */
class NetworkController(application: RunnableApplication) extends Actor with ScorexLogging {

  import NetworkController._

  lazy val localAddress = new InetSocketAddress(InetAddress.getByName(settings.bindAddress), settings.port)

  lazy val ownSocketAddress = getDeclaredHost.flatMap(host => Try(InetAddress.getByName(host)).toOption)
    .orElse {
      if (settings.upnpEnabled) application.upnp.externalAddress else None
    }.map(inetAddress => new InetSocketAddress(inetAddress, getDeclaredPort.getOrElse(application.settings.port)))

  lazy val connTimeout = Some(new FiniteDuration(settings.connectionTimeout, SECONDS))

  private lazy val settings = application.settings

  // there is not recovery for broken connections
  override val supervisorStrategy = SupervisorStrategy.stoppingStrategy

  private implicit val system = context.system


  private val peerManager = application.peerManager

  private val messageHandlers = mutable.Map[Seq[Message.MessageCode], ActorRef]()

  private var listeningStopped: Boolean = false
  private var conncetionClosed: Boolean = false
  private var maybeRequester: Option[ActorRef] = None

  //check own declared address for validity
  if (!settings.localOnly) {
    getDeclaredHost.forall { myHost =>
      Try {
        val myAddress = InetAddress.getAllByName(myHost)

        NetworkInterface.getNetworkInterfaces.exists { networkInterface =>
          networkInterface.getInterfaceAddresses.exists { interfaceAddress =>
            val externalAddress = interfaceAddress.getAddress
            myAddress.contains(externalAddress)
          }
        } match {
          case true => true
          case false =>
            if (settings.upnpEnabled) {
              val externalAddress = application.upnp.externalAddress
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
      val delay = if (settings.fuzzingDelay > 0) Random.nextInt(settings.fuzzingDelay) else 0
      system.scheduler.scheduleOnce(delay.millis) {
        peerManager ! stn
      }
  }

  def peerLogic: Receive = {
    case ConnectTo(remote) =>
      log.info(s"Connecting to: $remote")
      IO(Tcp) ! Connect(remote, localAddress = None, timeout = connTimeout)

    case Connected(remote, local) =>
      val connection = sender()
      createPeerHandler(connection, remote, inbound = false)

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
      conncetionClosed = true
      sendShutdownComplete()

    case ListeningStopped =>
      listeningStopped = true
      sendShutdownComplete()
  }

  private def sendShutdownComplete(): Unit = {
    if (conncetionClosed && listeningStopped) maybeRequester.foreach(_ ! NetworkShutdownComplete)
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse interfaceCalls orElse {
    case RegisterMessagesHandler(specs, handler) =>
      messageHandlers += specs.map(_.messageCode) -> handler

    case CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd)

    case nonsense: Any =>
      log.warn(s"NetworkController: got something strange $nonsense")
  }

  private def getDeclaredUri: Option[URI] =
    settings.declaredAddress.flatMap(declaredAddress => Try(new URI(s"http://$declaredAddress")).toOption)

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
      createPeerHandler(connection, remote, inbound = true)
  }

  private def createPeerHandler(connection: ActorRef, remote: InetSocketAddress, inbound: Boolean): Unit = {
    val handler = context.actorOf(Props(classOf[PeerConnectionHandler], application, connection, remote))
    peerManager ! PeerManager.Connected(remote, handler, ownSocketAddress, inbound)
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
