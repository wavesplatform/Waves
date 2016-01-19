package scorex.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp
import akka.io.Tcp._
import akka.util.{ByteString, CompactByteString}
import com.google.common.primitives.Ints
import scorex.app.Application
import scorex.network.NetworkController.PeerHandshake
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success}


case class ConnectedPeer(address: InetSocketAddress, handlerRef: ActorRef) {

  import shapeless.Typeable._

  override def equals(obj: scala.Any): Boolean = obj.cast[ConnectedPeer].exists(_.address == this.address)
}

//todo: timeout on Ack waiting
case class PeerConnectionHandler(application: Application,
                                 connection: ActorRef,
                                 remote: InetSocketAddress,
                                 ownNonce: Long) extends Actor with Buffering with ScorexLogging {

  import PeerConnectionHandler._

  context watch connection

  private lazy val networkControllerRef: ActorRef = application.networkController

  val selfPeer = new ConnectedPeer(remote, self)

  private def processErrors: Receive = {
    case CommandFailed(w: Write) =>
      log.info(s"Write failed : $w " + remote)
      //todo: blacklisting
      //peerManager.blacklistPeer(remote)
      connection ! Close

    case cc: ConnectionClosed =>
      networkControllerRef ! NetworkController.PeerDisconnected(remote)
      log.info("Connection closed to : " + remote + ": " + cc.getErrorCause)

    case CloseConnection =>
      log.info(s"Enforced to abort communication with: " + remote)
      connection ! Close
  }

  private def processOwnHandshake(newCycle: Receive): Receive = ({
    case h: Handshake =>
      connection ! Write(ByteString(h.bytes))
      context become newCycle
  }: Receive) orElse processErrors

  private def processHandshakeAck(newCycle: Receive): Receive = ({
    case Received(data) if data.length == HandShakeAck.messageSize =>
      if (data == HandShakeAck.bytes.toSeq) context become newCycle else connection ! Close
  }: Receive) orElse processErrors

  private def processHandshake(newCycle: Receive): Receive = ({
    case Received(data) if data.length > HandShakeAck.messageSize =>
      Handshake.parse(data.toArray) match {
        case Success(handshake) =>
          if (handshake.fromNonce != ownNonce) {
            connection ! Write(HandShakeAck.bytesAsByteString)
            networkControllerRef ! PeerHandshake(remote, handshake)
            context become newCycle
          } else {
            connection ! Close
          }
        case Failure(t) =>
          log.info(s"Error during parsing a handshake: $t")
          connection ! Close
      }
  }: Receive) orElse processErrors

  def workingCycleLocalInterface: Receive = {
    case msg: message.Message[_] =>
      val bytes = msg.bytes
      connection ! Write(ByteString(Ints.toByteArray(bytes.length) ++ bytes))

    case Blacklist =>
      log.info(s"Going to blacklist " + remote)
    //todo: real blacklisting
    //  PeerManager.blacklistPeer(remote)
    //  connection ! Close
  }

  private var chunksBuffer: ByteString = CompactByteString()

  def workingCycleRemoteInterface: Receive = {
    case Received(data) =>

      val t = getPacket(chunksBuffer ++ data)
      chunksBuffer = t._2

      t._1.find { packet =>
        application.messagesHandler.parse(packet.toByteBuffer, Some(selfPeer)) match {
          case Success(message) =>
            log.info("received message " + message.spec + " from " + remote)
            networkControllerRef ! message
            false

          case Failure(e) =>
            log.info(s"Corrupted data from: " + remote, e)
          //  connection ! Close
          //  context stop self
            true
        }
      }

    case CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd)

    case nonsense: Any => log.warn(s"Strange input for PeerConnectionHandler: $nonsense")
  }

  def workingCycle: Receive = workingCycleLocalInterface orElse workingCycleRemoteInterface orElse processErrors

  override def receive: Receive =
    processOwnHandshake(
      processHandshakeAck(processHandshake(workingCycle))
        orElse processHandshake(processHandshakeAck(workingCycle))
    ) orElse processHandshake(processOwnHandshake(processHandshakeAck(workingCycle)))
}

object PeerConnectionHandler {

  case object CloseConnection

  case object Blacklist
}
