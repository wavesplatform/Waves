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


//todo: timeout on Ack waiting (perhaps not needed since absence of Ack ==  TCP connection t/o)
case class PeerConnectionHandler(application: RunnableApplication,
                                 connection: ActorRef,
                                 remote: InetSocketAddress) extends Actor with Buffering with ScorexLogging {
  import PeerConnectionHandler._

  case object Ack extends Event
  private object HandshakeCheck

  private lazy val networkControllerRef: ActorRef = application.networkController
  private lazy val peerManager: ActorRef = application.peerManager

  private lazy val outboundBufferSize = math.max(
    application.settings.outboundBufferSize,
    application.settings.forkMaxLength + application.basicMessagesSpecsRepo.specs.size * 10)

  private val selfPeer = ConnectedPeer(remote, self)

  private var handshakeGot = false
  private var handshakeSent = false

  context watch connection

  override def preStart: Unit = connection ! ResumeReading

  override def postRestart(thr: Throwable): Unit = {
    log.warn(s"Restart because of $thr.getMessage")
    connection ! Close
  }

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
        case Failure(e) =>
          log.warn(s"Error during parsing a handshake from $remote", e)
          //todo: blacklist?
          connection ! Close
      }

    case HandshakeCheck =>
      if (handshakeGot && handshakeSent) {
        connection ! ResumeReading
        context become workingCycle
      }
  }: Receive) orElse processErrors(CommunicationState.AwaitingHandshake.toString)


  /**
    * Akka message handler when we don't wait ACK of last Write operation.
    * */
  private def workingCycleInterface: Receive = {
    case msg: message.Message[_] =>
      log.trace("Sending message " + msg.spec + " to " + remote)
      val bytes = msg.bytes
      val data = ByteString(Ints.toByteArray(bytes.length) ++ bytes)
      buffer(data)
      connection ! Write(data, Ack)
      context become workingCycleWaitingAck

    case Received(data: ByteString) =>
      processReceivedData(data)
      connection ! ResumeReading
  }

  /**
    * Akka message handler when we wait ACK of last Write.
    * */
  private def workingCycleWaitingAckInterface: Receive = {
    case msg: message.Message[_] =>
      log.trace(s"Buffering outbound message " + msg.spec + " to " + remote)
      val bytes = msg.bytes
      val data = ByteString(Ints.toByteArray(bytes.length) ++ bytes)
      buffer(data)

    case Ack => acknowledge()

    case Received(data: ByteString) =>
      processReceivedData(data)
      connection ! ResumeReading
  }

  private def workingCycleWaitingWritingResumedInterface: Receive = {
    case msg: message.Message[_] =>
      log.trace(s"Buffering outbound message " + msg.spec + " to " + remote)
      val bytes = msg.bytes
      val data = ByteString(Ints.toByteArray(bytes.length) ++ bytes)
      buffer(data)

    case WritingResumed =>
      log.trace("WritingResumed")
      connection ! Write(outboundBuffer(0), Ack)

    case Received(data: ByteString) =>
      processReceivedData(data)
      connection ! ResumeReading
  }

  private def blackListingInterface: Receive = {
    case Blacklist =>
      log.info(s"Going to blacklist " + remote)
      peerManager ! AddToBlacklist(remote)
      connection ! Close
  }

  private var outboundBuffer = Vector.empty[ByteString]

  private def buffer(data: ByteString) = {
    outboundBuffer :+= data
    if (outboundBuffer.length > outboundBufferSize) {
      log.warn(s"Drop connection to $remote : outbound buffer overrun")
      connection ! Close
    }
  }

  private def acknowledge() = {
    require(outboundBuffer.nonEmpty, "outbound buffer was empty")

    outboundBuffer = outboundBuffer.drop(1)

    if (outboundBuffer.isEmpty) {
      log.trace(s"Outbound buffer for $remote is empty. Going to ${CommunicationState.WorkingCycle.toString}")
      context become workingCycle
    } else {
      log.trace(s"Sending message from outbound buffer to $remote. Outbound buffer size: ${outboundBuffer.length}")
      connection ! Write(outboundBuffer(0), Ack)
    }
  }

  private def workingCycle: Receive =
    workingCycleInterface orElse blackListingInterface orElse
      processErrors(CommunicationState.WorkingCycle.toString) orElse {
      case nonsense: Any =>
        log.warn(s"Strange input for PeerConnectionHandler: $nonsense")
    }

  private def workingCycleWaitingAck: Receive =
    workingCycleWaitingAckInterface orElse blackListingInterface orElse
      processErrors(CommunicationState.WorkingCycleWaitingAck.toString) orElse {
      case nonsense: Any =>
        log.warn(s"Strange input for PeerConnectionHandler: $nonsense")
    }

  private def workingCycleWaitingWritingResumed: Receive =
    workingCycleWaitingWritingResumedInterface orElse blackListingInterface orElse
      processErrors(CommunicationState.WorkingCycleWaitingAck.toString) orElse {
      case nonsense: Any =>
        log.warn(s"Strange input for PeerConnectionHandler: $nonsense")
    }

  private def processErrors(stateName: String): Receive = {
    case CommandFailed(w: Write) =>
      log.warn(s"Write failed :$w " + remote + s" in state $stateName")
      connection ! ResumeWriting
      context become workingCycleWaitingWritingResumed

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
  }

  private var chunksBuffer: ByteString = CompactByteString()

  private def processReceivedData(data: ByteString) = {
    val (pkt, remainder) = getPacket(chunksBuffer ++ data)
    chunksBuffer = remainder

    pkt.find { packet =>
      application.messagesHandler.parseBytes(packet.toByteBuffer, Some(selfPeer)) match {
        case Success(message) =>
          log.trace("Received message " + message.spec + " from " + remote)
          networkControllerRef ! message
          false

        case Failure(e) =>
          log.warn(s"Corrupted data from: " + remote, e)
          //  connection ! Close
          //  context stop self
          true
      }
    }
  }
}

object PeerConnectionHandler {

  private object CommunicationState extends Enumeration {
    type CommunicationState = Value

    val AwaitingHandshake = Value("AwaitingHandshake")
    val WorkingCycle = Value("WorkingCycle")
    val WorkingCycleWaitingAck = Value("WorkingCycleWaitingAck")
  }

  case object CloseConnection

  case object Blacklist
}