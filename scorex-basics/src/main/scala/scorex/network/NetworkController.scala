package scorex.network

import java.net.{InetAddress, InetSocketAddress, NetworkInterface, URI}

import akka.actor._
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import scorex.app.Application
import scorex.network.message.{Message, MessageSpec}
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}


//must be singleton
class NetworkController(application: Application) extends Actor with ScorexLogging {

  import NetworkController._

  private implicit val system = context.system

  private lazy val settings = application.settings
  private lazy val peerManager = application.peerManager

  private val connectedPeers = mutable.Map[ConnectedPeer, Option[Handshake]]()
  private var connectingPeer: Option[InetSocketAddress] = None

  private val messageHandlers = mutable.Map[Seq[Message.MessageCode], ActorRef]()

  lazy val nodeNonce:Long = (Random.nextInt() + 1000) * Random.nextInt() + Random.nextInt()

  val handshakeTemplate = Handshake(application.applicationName,
    application.appVersion,
    ownAddress.toString,
    settings.port,
    nodeNonce,
    0
  )

  //check own declared address for validity
  if (!settings.localOnly) {
    settings.declaredAddress.map { myAddress =>
      Try {
        val uri = new URI("http://" + myAddress)
        val myHost = uri.getHost
        val myAddrs = InetAddress.getAllByName(myHost)

        NetworkInterface.getNetworkInterfaces.exists { intf =>
          intf.getInterfaceAddresses.exists { intfAddr =>
            val extAddr = intfAddr.getAddress
            myAddrs.contains(extAddr)
          }
        } match {
          case true => true
          case false =>
            if (settings.upnpEnabled) {
              val extAddr = application.upnp.externalAddress
              myAddrs.contains(extAddr)
            } else false
        }
      }.recover { case t: Throwable =>
        log.error("Declared address validation failed: ", t)
        false
      }.getOrElse(false)
    }.getOrElse(true).ensuring(_ == true, "Declared address isn't valid")
  }

  lazy val externalAddress = settings.declaredAddress.map(InetAddress.getByName).orElse {
    if (settings.upnpEnabled) application.upnp.externalAddress else None
  }

  externalAddress.foreach { declaredAddress =>
    peerManager.addPeer(new InetSocketAddress(declaredAddress, settings.port))
  }

  log.info(s"Declared address: ${settings.declaredAddress}")

  lazy val localAddress = new InetSocketAddress(InetAddress.getByName(settings.bindAddress), settings.port)
  lazy val connTimeout = Some(new FiniteDuration(settings.connectionTimeout, SECONDS))

  IO(Tcp) ! Bind(self, localAddress)

  //address to send to ther peers
  lazy val ownAddress = externalAddress.getOrElse(localAddress.getAddress)


  private def bindingLogic: Receive = {
    case b@Bound(localAddr) =>
      log.info("Successfully bound to the port " + settings.port)
      context.system.scheduler.schedule(200.millis, 3.seconds)(self ! CheckPeers)

    case CommandFailed(_: Bind) =>
      log.error("Network port " + settings.port + " already in use!")
      context stop self
      application.stopAll()
  }

  def businessLogic: Receive = {
    case RegisterMessagesHandler(specs, handler) =>
      messageHandlers += specs.map(_.messageCode) -> handler

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
            //todo: ???
          }
        case Failure(e) =>
          log.error("Failed to deserialize data: " + e.getMessage)
        //todo: ban peer
      }

    case SendToNetwork(message, sendingStrategy) =>
      sendingStrategy.choose(connectedPeers.keys.toSeq).foreach(_.handlerRef ! message)
  }

  def peerLogic: Receive = {
    case CheckPeers =>
      if (connectedPeers.size < settings.maxConnections && connectingPeer.isEmpty) {
        peerManager.randomPeer().foreach { peer =>
          if (!connectedPeers.map(_._1.address).contains(peer)) {
            connectingPeer = Some(peer)
            IO(Tcp) ! Connect(peer, localAddress = None, timeout = connTimeout)
          }
        }
      }

    case c@Connected(remote, local) =>
      val connection = sender()
      val handler = context.actorOf(Props(classOf[PeerConnectionHandler], application, connection, remote, nodeNonce))
      connection ! Register(handler)
      val newPeer = new ConnectedPeer(remote, handler)
      connectedPeers += newPeer -> None

      newPeer.handlerRef ! handshakeTemplate.copy(time = System.currentTimeMillis() / 1000)

      if (connectingPeer.contains(remote)) {
        log.info(s"Connected to $remote, local is: $local")
        connectingPeer = None
        peerManager.onPeerConnected(remote)
      } else {
        log.info(s"Got incoming connection from $remote")
      }

    case CommandFailed(c: Connect) =>
      log.info("Failed to connect to : " + c.remoteAddress)
      connectingPeer = None
      peerManager.onPeerDisconnected(c.remoteAddress)

    case PeerDisconnected(remote) =>
      connectedPeers.retain{case (p,_) => p.address != remote}
      peerManager.onPeerDisconnected(remote)

    case PeerHandshake(address, handshake) =>
      connectedPeers.find(_._1.address == address).foreach{case (cp, _) =>
        connectedPeers.update(cp, Some(handshake))
      }
  }

  //calls from API / application
  def interfaceCalls: Receive = {
    case ShutdownNetwork =>
      log.info("Going to shutdown all connections & unbind port")
      connectedPeers.keys.foreach(_.handlerRef ! PeerConnectionHandler.CloseConnection)
      self ! Unbind
      context stop self

    case GetConnectedPeers => sender() ! (connectedPeers.values.flatten.toSeq:Seq[Handshake])
  }

  override def receive: Receive = bindingLogic orElse businessLogic orElse peerLogic orElse interfaceCalls orElse {
    case CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd)

    case nonsense: Any => log.warn(s"NetworkController: got something strange $nonsense")
  }
}

object NetworkController {

  case class RegisterMessagesHandler(specs: Seq[MessageSpec[_]], handler: ActorRef)

  //todo: more stricter solution for messageType than number?
  case class DataFromPeer[V](messageType: Message.MessageCode, data: V, source: ConnectedPeer)

  case class SendToNetwork(message: Message[_], sendingStrategy: SendingStrategy)

  private case object CheckPeers

  case object ShutdownNetwork

  case object GetConnectedPeers

  case class PeerDisconnected(address: InetSocketAddress)

  case class PeerHandshake(address: InetSocketAddress, handshake: Handshake)

}
