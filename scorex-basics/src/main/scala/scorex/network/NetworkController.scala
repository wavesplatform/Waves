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
import scala.util.{Failure, Success, Try}


//must be singleton
class NetworkController(application: Application) extends Actor with ScorexLogging {

  import NetworkController._

  private implicit val system = context.system

  private lazy val settings = application.settings
  private lazy val peerManager = application.peerManager

  private val connectedPeers = mutable.Map[InetSocketAddress, ConnectedPeer]()
  private val connectingPeers = mutable.Buffer[InetSocketAddress]()

  private val messageHandlers = mutable.Map[Seq[Message.MessageCode], ActorRef]()

  //check own declared address for validity
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

  log.info(s"Declared address: ${settings.declaredAddress}")

  IO(Tcp) ! Bind(self, new InetSocketAddress(InetAddress.getByName(settings.bindAddress), settings.port))


  override def receive: Receive = {
    case b@Bound(localAddress) =>
      log.info("Successfully bound to the port " + settings.port)
      context.system.scheduler.schedule(200.millis, 3.seconds)(self ! CheckPeers)

    case CommandFailed(_: Bind) =>
      log.error("Network port " + settings.port + " already in use!")
      context stop self
      application.stopAll()

    case CheckPeers =>
      if (connectedPeers.size < settings.maxConnections) {
        peerManager.randomPeer() match {
          case Some(peer) =>
            if (!connectedPeers.contains(peer) && !connectingPeers.contains(peer)) {
              connectingPeers += peer
              val connTimeout = Some(new FiniteDuration(settings.connectionTimeout, SECONDS))
              IO(Tcp) ! Connect(peer, timeout = connTimeout)
            }
          case None =>
        }
      }

    //if check as Connected is being sent on Bind also
    case c@Connected(remote, local) =>

      log.info(s"Connected to $remote, local is: $local")
      connectingPeers -= remote
      val connection = sender()
      val handler = context.actorOf(Props(classOf[PeerConnectionHandler], application, connection, remote))
      connection ! Register(handler)
      connectedPeers += remote -> new ConnectedPeer(remote, handler)
      peerManager.peerConnected(remote)

    case CommandFailed(c: Connect) =>
      log.info("Failed to connect to : " + c.remoteAddress)
      connectedPeers -= c.remoteAddress
      peerManager.peerDisconnected(c.remoteAddress)

    case CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd)

    case ShutdownNetwork =>
      log.info("Going to shutdown all connections & unbind port")
      connectedPeers.values.foreach(_.handlerRef ! PeerConnectionHandler.CloseConnection)
      self ! Unbind
      context stop self

    case PeerDisconnected(remote) =>
      connectedPeers -= remote
      peerManager.peerDisconnected(remote)


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
      sendingStrategy.choose(connectedPeers.values.toSeq).foreach(_.handlerRef ! message)

    case GetPeers => sender() ! connectedPeers.values.toSeq

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

  case object GetPeers

  case class PeerDisconnected(address: InetSocketAddress)

}
