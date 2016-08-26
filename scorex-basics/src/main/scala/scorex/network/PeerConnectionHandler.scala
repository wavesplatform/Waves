package scorex.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, SupervisorStrategy}
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

  context watch connection

  override def preStart: Unit = connection ! ResumeReading

  override def postRestart(thr: Throwable): Unit = {
    log.warn(s"Restart because of $thr.getMessage")
    connection ! Close
  }

  // there is not recovery for broken connections
  override val supervisorStrategy = SupervisorStrategy.stoppingStrategy

  override def receive: Receive = state(CommunicationState.AwaitingHandshake) {
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

    case HandshakeTimeout =>
      log.warn(s"Handshake timeout for $remote")
      connection ! Close

    case HandshakeCheck =>
      if (handshakeGot && handshakeSent) {
        timeout.cancel()
        connection ! ResumeReading
        context become workingCycle
      }
  }

  private def buffer(data: ByteString) = {
    outboundBuffer :+= data
    if (outboundBuffer.map(_.size).sum > outboundBufferSize) {
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
      case HandshakeCheck =>

      case nonsense: Any => log.warn(s"Strange input in state $state: $nonsense")
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
      application.messagesHandler.parseBytes(packet.toByteBuffer) match {
        case Success((spec, msgData)) =>
          log.trace("Received message " + spec + " from " + remote)
          peerManager ! RawNetworkData(spec, msgData, remote)
          false

        case Failure(e) =>
          log.warn(s"Corrupted data from: " + remote, e)
          //  connection ! Close
          //  context stop self
          true
      }
    }

    connection ! ResumeReading
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

  private case object HandshakeCheck
  private case object HandshakeTimeout

  case object CloseConnection
}