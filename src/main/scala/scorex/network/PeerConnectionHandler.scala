package scorex.network

import java.net.InetSocketAddress
import akka.actor.{Actor, ActorRef, Status, Terminated}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import com.google.common.primitives.Ints
import scorex.app.RunnableApplication
import scorex.network.message.MessageHandler.RawNetworkData
import scorex.network.peer.PeerManager
import scorex.network.peer.PeerManager.Handshaked
import scorex.utils.ScorexLogging
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.{Failure, Success}

case class PeerConnectionHandler(application: RunnableApplication,
                                 connection: ActorRef,
                                 remote: InetSocketAddress) extends Actor with Buffering with ScorexLogging {

  import PeerConnectionHandler._

  private lazy val peerManager = application.peerManager

  private lazy val outboundBufferSize = application.settings.outboundBufferSize

  private var outboundBuffer = Vector.empty[ByteString]

  private var handshakeGot = false
  private var handshakeSent = false

  private val timeout = context.system.scheduler.scheduleOnce(
    application.settings.connectionTimeout seconds, self, HandshakeTimeout)

  connection ! Register(self, keepOpenOnPeerClosed = false, useResumeWriting = true)

  context watch connection

  override def postStop(): Unit = {
    log.debug(s"Disconnected from $remote")
    peerManager ! PeerManager.Disconnected(remote)
    timeout.cancel()
  }

  override def receive: Receive = state(CommunicationState.AwaitingHandshake) {
    case h: Handshake =>
      connection ! Write(ByteString(h.bytes), Ack)
      log.debug(s"Handshake message has been sent to $remote")

    case Ack =>
      log.info(s"Handshake sent to $remote")
      handshakeSent = true
      checkHandshake()

    case Received(data) =>
      Handshake.parseBytes(data.toArray) match {
        case Success(handshake) =>
          peerManager ! Handshaked(remote, handshake)
          log.info(s"Got a Handshake from $remote")
          handshakeGot = true
          checkHandshake()
        case Failure(e) =>
          log.warn(s"Error during parsing a handshake from $remote: ${e.getMessage}")
          context stop self
      }

    case HandshakeTimeout =>
      log.warn(s"Handshake timeout for $remote")
      context stop self
  }

  /**
    * Checks that we've sent and received handshakes. If so switch context to working cycle
    */
  private def checkHandshake() = {
    if (handshakeGot && handshakeSent) {
      timeout.cancel()
      context become workingCycle
    }
  }

  private def buffer(data: ByteString) = {
    if (outboundBuffer.map(_.size).sum + outboundBuffer.size < outboundBufferSize) {
      outboundBuffer :+= data
    } else {
      log.warn(s"Drop connection to $remote : outbound buffer overrun")
      context stop self
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

  private def workingCycle: Receive = state(CommunicationState.WorkingCycle) {
    case msg: message.Message[_] =>
      log.trace("Sending message " + msg.spec + " to " + remote)
      val bytes = msg.bytes
      val data = ByteString(Ints.toByteArray(bytes.length) ++ bytes)
      buffer(data)
      connection ! Write(data, Ack)
      context become workingCycleWaitingAck

    case Received(data: ByteString) => processReceivedData(data)
  }

  private def workingCycleWaitingAck: Receive = state(CommunicationState.WorkingCycleWaitingAck) {
    case msg: message.Message[_] =>
      log.trace(s"Buffering outbound message " + msg.spec + " to " + remote)
      val bytes = msg.bytes
      val data = ByteString(Ints.toByteArray(bytes.length) ++ bytes)
      buffer(data)

    case Ack => acknowledge()

    case Received(data: ByteString) => processReceivedData(data)
  }

  private def workingCycleWaitingWritingResumed: Receive = state(CommunicationState.WorkingCycleWaitingWritingResumed) {
    case msg: message.Message[_] =>
      log.trace(s"Buffering outbound message " + msg.spec + " to " + remote)
      val bytes = msg.bytes
      val data = ByteString(Ints.toByteArray(bytes.length) ++ bytes)
      buffer(data)

    case WritingResumed =>
      log.trace("WritingResumed")
      connection ! Write(outboundBuffer(0), Ack)

    case Received(data: ByteString) => processReceivedData(data)
  }

  private def state(state: CommunicationState.Value)(logic: Receive): Receive =
    logic orElse processErrors(state.toString) orElse {
      case HandshakeTimeout =>

      case nonsense: Any => log.warn(s"Strange input in state $state: $nonsense")
    }

  private def processErrors(stateName: String): Receive = {
    case CommandFailed(w: Write) =>
      log.warn(s"Write failed :$w " + remote + s" in state $stateName")
      connection ! ResumeWriting
      context become workingCycleWaitingWritingResumed

    case cc: ConnectionClosed =>
      val reason = if (cc.isErrorClosed) cc.getErrorCause else if (cc.isPeerClosed) "by remote" else s"${cc.isConfirmed} - ${cc.isAborted}"
      log.info(s"Connection closed to $remote: $reason in state $stateName")
      context stop self

    case Terminated(terminatedActor) if terminatedActor == connection =>
      log.info(s"Connection to $remote terminated")
      context stop self

    case CloseConnection =>
      log.info(s"Enforced to close communication with: $remote in state $stateName")
      sender() ! CloseConnectionCompleted(remote)
      context stop self

    case CommandFailed(cmd: Tcp.Command) =>
      log.warn(s"Failed to execute command: $cmd in state $stateName")
  }

  private var chunksBuffer: ByteString = CompactByteString()

  private def processReceivedData(data: ByteString) = {
    val (pkt, remainder) = getPacket(chunksBuffer ++ data)
    chunksBuffer = remainder

    pkt.find { packet =>
      application.messagesHandler.parseBytes(packet.toByteBuffer) match {
        case Success((spec, msgData)) =>
          log.trace("Received message " + spec + " from " + remote)
          peerManager ! RawNetworkData(spec, msgData, remote)
          false

        case Failure(e) =>
          log.error(s"Can't parse message from " + remote + " : " + e.getMessage)
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
    val WorkingCycleWaitingWritingResumed = Value("WorkingCycleWaitingWritingResumed")
  }

  private case object Ack extends Event

  private case object HandshakeTimeout

  case object CloseConnection

  case class CloseConnectionCompleted(remote: InetSocketAddress)
}
