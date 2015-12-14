package scorex.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp._
import akka.util.ByteString
import scorex.app.Application
import scorex.utils.ScorexLogging

import scala.util.{Failure, Success}


class ConnectedPeer(val address: InetSocketAddress, val handlerRef: ActorRef) {

  import shapeless.Typeable._

  override def equals(obj: scala.Any): Boolean = obj.cast[ConnectedPeer].exists(_.address == this.address)

  override def toString: String = super.toString
}

case class PeerConnectionHandler(application: Application,
                                 connection: ActorRef,
                                 remote: InetSocketAddress) extends Actor with ScorexLogging {

  import PeerConnectionHandler._

  context watch connection

  private lazy val networkControllerRef: ActorRef = application.networkController

  val selfPeer = new ConnectedPeer(remote, self)


  //  context.system.scheduler.schedule(1.second, 5.seconds)(self ! SendBlockchainScore)

  /*
  private def handleMessage(message: message.Message[_]) = {
    log.debug("Handling message: " + message)
    message match {

      case SignaturesMessage(signaturesGot) =>
        log.info(s"Got SignaturesMessage with ${signaturesGot.length} sigs")

        val common = signaturesGot.head
        require(blockchainStorage.contains(common))

        blockchainStorage.removeAfter(common)

        signaturesGot.tail.foreach { case sig =>
          self ! GetBlockMessage(sig)
        }

      case GetBlockMessage(signature) =>
        blockchainStorage.blockById(signature) match {
          case Some(block) => self ! BlockMessage(blockchainStorage.heightOf(block).get, block)
          case None => self ! Blacklist
        }

      case BlockMessage(height, block) =>
        require(block != null)
        log.info(s"Got block, height $height , local height: " + blockchainStorage.height())

        if (height == blockchainStorage.height() + 1) {
          if (block.isValid)
            networkController ! NewBlock(block, Some(remote))
          else
            log.info(s"Got non-valid block (height of a block: $height")
        }

      case TransactionMessage(transaction) =>
        if (!transaction.isSignatureValid || transaction.transactionType == TransactionType.GenesisTransaction) {
          self ! Blacklist
        } else if (transaction.hasMinimumFee && transaction.hasMinimumFeePerByte) {
          application.onNewOffchainTransaction(transaction)
        }

      case nonsense: Any => log.warn(s"PeerConnectionHandler: got something strange $nonsense")
    }
  } */

  override def receive = {

    //    case SendBlockchainScore =>
    //      self ! ScoreMessage(blockchainStorage.height(), blockchainStorage.score())

    case msg: message.Message[_] =>
      connection ! Write(ByteString(msg.bytes))

    case CommandFailed(w: Write) =>
      log.info(s"Write failed : $w " + remote)
      //todo: blacklisting
      //peerManager.blacklistPeer(remote)
      connection ! Close

    case Received(data) =>
      application.messagesHandler.parse(data.toByteBuffer, Some(selfPeer)) match {
        case Success(message) =>
          log.info("received message " + message.spec + " from " + remote)
          networkControllerRef ! message

        case Failure(e) =>
          log.info(s"Corrupted data from: " + remote + " : " + e.getMessage)
          connection ! Close
        //context stop self
      }

    case cc: ConnectionClosed =>
      networkControllerRef ! NetworkController.PeerDisconnected(remote)
      log.info("Connection closed to : " + remote + ": " + cc.getErrorCause)

    case CloseConnection =>
      log.info(s"Enforced to abort communication with: " + remote)
      connection ! Close

    case Blacklist =>
      log.info(s"Going to blacklist " + remote)
    //todo: real blacklisting
    //  PeerManager.blacklistPeer(remote)
    //  connection ! Close

    case nonsense: Any => log.warn(s"Strange input for PeerConnectionHandler: $nonsense")
  }
}

object PeerConnectionHandler {

  //  case object SendBlockchainScore

  case object CloseConnection

  case object Blacklist

}