package scorex.network

import java.net.InetSocketAddress
import java.util.logging.Logger

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp._
import akka.util.ByteString
import scorex.block.{Block, NewBlock}
import scorex.database.UnconfirmedTransactionsDatabaseImpl
import scorex.database.blockchain.PrunableBlockchainStorage
import scorex.network.NetworkController.UpdateHeight
import scorex.network.message.{Message, _}
import scorex.transaction.Transaction.TransactionType

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}


class PeerConnectionHandler(networkController: ActorRef,
                            connection: ActorRef,
                            remote: InetSocketAddress) extends Actor {

  import PeerConnectionHandler._

  private var best = false

  private case class MessagesWithoutReply(pingAwait: Boolean, peersAwait: Boolean, sigsAwait: Boolean)

  private var flags = MessagesWithoutReply(pingAwait = false, peersAwait = false, sigsAwait = false)

  context watch connection

  context.system.scheduler.schedule(500.millis, 3.seconds)(self ! PingRemote)
  context.system.scheduler.schedule(1.second, 15.seconds)(self ! SendHeight)

  private def handleMessage(message: Message) = {
    message match {
      case PingMessage =>
        flags.copy(pingAwait = false)
        self ! PingMessage

      case GetPeersMessage =>
        self ! PeersMessage(PeerManager.knownPeers())

      case PeersMessage(peers) =>
        flags.copy(peersAwait = false)
        println("got peers: " + peers) //todo:handling

      case HeightMessage(height) => networkController ! UpdateHeight(remote, height)

      case GetSignaturesMessage(signaturesGot) =>
        signaturesGot.exists { parent =>
          val headers = PrunableBlockchainStorage.getSignatures(parent)
          if (headers.size > 0) {
            self ! SignaturesMessage(Seq(parent) ++ headers)
            true
          } else false
        }

      case SignaturesMessage(signaturesGot) =>
        flags.copy(sigsAwait = false)
        val common = signaturesGot.head
        require(PrunableBlockchainStorage.contains(common))

        PrunableBlockchainStorage.removeAfter(common)

        signaturesGot.tail.foreach { case sig =>
          self ! GetBlockMessage(sig)
        }

      case GetBlockMessage(signature) =>
        PrunableBlockchainStorage.blockByHeader(signature) match {
          case Some(block) => self ! BlockMessage(block.height().get, block)
          case None => self ! Blacklist
        }

      case BlockMessage(height, block) =>
        require(block != null)
        Logger.getGlobal.info(s"Got block, height $height , local height: " + PrunableBlockchainStorage.height())

        if (Block.isNewBlockValid(block)) {
          networkController ! NewBlock(block, Some(remote))
        } else networkController ! UpdateHeight(remote, height)

      case TransactionMessage(transaction) =>
        //CHECK IF SIGNATURE IS VALID OR GENESIS TRANSACTION
        if (!transaction.isSignatureValid || transaction.transactionType == TransactionType.GENESIS_TRANSACTION) {
          self ! Blacklist
        } else if (transaction.hasMinimumFee && transaction.hasMinimumFeePerByte) {
          UnconfirmedTransactionsDatabaseImpl.put(transaction)
          networkController ! NetworkController.BroadcastMessage(message, List(remote))
        }

      case a: Any => Logger.getGlobal.warning(s"PeerConnectionHandler: got something strange $a")
    }
  }

  override def receive = {
    case PingRemote => self ! PingMessage

    case SendHeight => self ! HeightMessage(PrunableBlockchainStorage.height())

    case msg: Message =>
      val (sendFlag, newFlags) = msg match {
        case PingMessage =>
          (!flags.pingAwait, flags.copy(pingAwait = true))
        case GetPeersMessage =>
          (!flags.peersAwait, flags.copy(peersAwait = true))
        //case _:GetSignaturesMessage =>
        //(!flags.sigsAwait, flags.copy(sigsAwait = true))
        case _ =>
          (true, flags)
      }
      if (sendFlag) {
        self ! ByteString(msg.toBytes())
        flags = newFlags
      }

    case data: ByteString =>
      connection ! Write(data)

    case CommandFailed(w: Write) =>
      // O/S buffer was full
      Logger.getGlobal.info(s"Write failed : $w " + remote)
      PeerManager.blacklistPeer(remote)
      connection ! Close

    case Received(data) =>
      Message.parse(data.toByteBuffer) match {
        case Failure(e) =>
          Logger.getGlobal.info(s"Corrupted data from: " + remote + " : " + e.getMessage)
          connection ! Close
        //context stop self

        case Success(message) =>
          Logger.getGlobal.info("received message " + message.messageType + " from " + remote)

          handleMessage(message)
      }

    case cc: ConnectionClosed =>
      networkController ! NetworkController.PeerDisconnected(remote)
      Logger.getGlobal.info("Connection closed to : " + remote + ": " + cc.getErrorCause)

    case CloseConnection =>
      Logger.getGlobal.info(s"Enforced to abort communication with: " + remote)
      connection ! Close

    case Blacklist =>
      Logger.getGlobal.info(s"Going to blacklist " + remote)
    //  PeerManager.blacklistPeer(remote)
    //  connection ! Close

    case BestPeer(peer, betterThanLocal) => best = betterThanLocal && (peer == remote)
  }
}

object PeerConnectionHandler {

  case object PingRemote

  case object SendHeight

  case object CloseConnection

  case object Blacklist

  case class BestPeer(remote: InetSocketAddress, betterThanLocal: Boolean)

}