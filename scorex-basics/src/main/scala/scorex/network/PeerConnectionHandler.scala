package scorex.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, SupervisorStrategy}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import com.google.common.primitives.Ints
import scorex.app.RunnableApplication
import scorex.network.peer.PeerManager
import scorex.network.peer.PeerManager.{AddToBlacklist, Handshaked}
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success}

case class ConnectedPeer(socketAddress: InetSocketAddress, handlerRef: ActorRef) {
  import shapeless.syntax.typeable._

  override def equals(obj: Any): Boolean =
    obj.cast[ConnectedPeer].exists(_.socketAddress == this.socketAddress)

  override def hashCode(): Int = socketAddress.hashCode()
  override def toString: String = socketAddress.toString
}


//todo: timeout on Ack waiting
case class PeerConnectionHandler(application: RunnableApplication,
                                 connection: ActorRef,
                                 remote: InetSocketAddress) extends Actor with Buffering with ScorexLogging {
  import PeerConnectionHandler._

  case object Ack extends Event
  private object HandshakeCheck

  private lazy val networkControllerRef: ActorRef = application.networkController
  private lazy val peerManager: ActorRef = application.peerManager
  private val selfPeer = new ConnectedPeer(remote, self)
  private var handshakeGot = false
  private var handshakeSent = false

  context watch connection

  override def preStart: Unit = connection ! ResumeReading

  // there is not recovery for broken connections
  override val supervisorStrategy = SupervisorStrategy.stoppingStrategy

  override def receive: Receive = handshakeCycle

  private def handshakeCycle: Receive = ({
    case h: Handshake =>
      connection ! Write(ByteString(h.bytes))
      log.info(s"Handshake sent to $remote")
      handshakeSent = true
      self ! HandshakeCheck

    case Received(data) =>
      Handshake.parseBytes(data.toArray) match {
        case Success(handshake) =>
          peerManager ! Handshaked(remote, handshake)
          log.info(s"Got a Handshake from $remote")
          connection ! ResumeReading
          handshakeGot = true
          self ! HandshakeCheck
        case Failure(throwable) =>
          log.warn(s"Error during parsing a handshake", throwable)
          //todo: blacklist?
          connection ! Close
      }

    case HandshakeCheck =>
      if (handshakeGot && handshakeSent) {
        connection ! ResumeReading
        context become workingCycle
      }
  }: Receive) orElse processErrors(CommunicationState.AwaitingHandshake.toString)

  private def processErrors(stateName: String): Receive = {
    case CommandFailed(w: Write) =>
      log.warn(s"Write failed :$w " + remote + s" in state $stateName")
//      peerManager ! AddToBlacklist(remote)
      connection ! Close

      connection ! ResumeReading
      connection ! ResumeWriting

    case cc: ConnectionClosed =>
      peerManager ! PeerManager.Disconnected(remote)
      val reason = if (cc.isErrorClosed) cc.getErrorCause else if (cc.isPeerClosed) "by remote" else s"${cc.isConfirmed} - ${cc.isAborted}"
      log.info(s"Connection closed to $remote: $reason in state $stateName")
      context stop self

    case CloseConnection =>
      log.info(s"Enforced to abort communication with: " + remote + s" in state $stateName")
      connection ! Close

    case CommandFailed(cmd: Tcp.Command) =>
      log.warn("Failed to execute command : " + cmd + s" in state $stateName")
      connection ! ResumeReading
  }

  private def workingCycleLocalInterface: Receive = {
    case msg: message.Message[_] =>
      log.debug("Sending message " + msg.spec + " to " + remote)
      val bytes = msg.bytes
      val data = ByteString(Ints.toByteArray(bytes.length) ++ bytes)
      connection ! Write(data)

    case Blacklist =>
      log.info(s"Going to blacklist " + remote)
      peerManager ! AddToBlacklist(remote)
      connection ! Close
  }

  private var chunksBuffer: ByteString = CompactByteString()

  private def workingCycleRemoteInterface: Receive = {
    case Received(data) =>

      val t = getPacket(chunksBuffer ++ data)
      chunksBuffer = t._2

      t._1.find { packet =>
        application.messagesHandler.parseBytes(packet.toByteBuffer, Some(selfPeer)) match {
          case Success(message) =>
            log.debug("Received message " + message.spec + " from " + remote)
            networkControllerRef ! message
            false

          case Failure(throwable) =>
            log.warn(s"Corrupted data from: " + remote, throwable)
            //  connection ! Close
            //  context stop self
            true
        }
      }
      connection ! ResumeReading
  }

  private def workingCycle: Receive =
    workingCycleLocalInterface orElse
      workingCycleRemoteInterface orElse
      processErrors(CommunicationState.WorkingCycle.toString) orElse ({
      case nonsense: Any =>
        log.warn(s"Strange input for PeerConnectionHandler: $nonsense")
    }: Receive)

}

object PeerConnectionHandler {

  private object CommunicationState extends Enumeration {
    type CommunicationState = Value

    val AwaitingHandshake = Value("AwaitingHandshake")
    val WorkingCycle = Value("WorkingCycle")
  }

  case object CloseConnection

  case object Blacklist

}