package scorex.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import akka.io.Tcp._
import akka.util.ByteString
import scorex.Controller
import scorex.block.{Block, NewBlock}
import scorex.network.NetworkController.UpdateBlockchainScore
import scorex.network.message.{Message, _}
import scorex.transaction.Transaction.TransactionType
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Success}


class PeerConnectionHandler(networkController: ActorRef,
                            connection: ActorRef,
                            remote: InetSocketAddress) extends Actor with ScorexLogging {

  import PeerConnectionHandler._

  private var best = false

  private case class MessagesWithoutReply(pingAwait: Boolean, peersAwait: Boolean, sigsAwait: Boolean)

  private var flags = MessagesWithoutReply(pingAwait = false, peersAwait = false, sigsAwait = false)

  context watch connection

  context.system.scheduler.schedule(500.millis, 10.seconds)(self ! PingRemote)
  context.system.scheduler.schedule(1.second, 15.seconds)(self ! SendBlockchainScore)

  private def handleMessage(message: Message) = {
    message match {
      case PingMessage =>
        flags.copy(pingAwait = false)
        self ! PingMessage

      case GetPeersMessage =>
        self ! PeersMessage(PeerManager.knownPeers())

      case PeersMessage(peers) =>
        flags.copy(peersAwait = false)
        peers.foreach { peer =>
          PeerManager.addPeer(peer)
        }

      case ScoreMessage(height, score) => networkController ! UpdateBlockchainScore(remote, height, score)

      case GetSignaturesMessage(signaturesGot) =>
        log.info(s"Got GetSignaturesMessage with ${signaturesGot.length} sigs within")

        signaturesGot.exists { parent =>
          val headers = Controller.blockchainStorage.getSignatures(parent)
          if (headers.nonEmpty) {
            self ! SignaturesMessage(Seq(parent) ++ headers)
            true
          } else false
        }

      case SignaturesMessage(signaturesGot) =>
        log.info(s"Got SignaturesMessage with ${signaturesGot.length} sigs")

        flags.copy(sigsAwait = false)
        val common = signaturesGot.head
        require(Controller.blockchainStorage.contains(common))

        Controller.blockchainStorage.removeAfter(common)

        signaturesGot.tail.foreach { case sig =>
          self ! GetBlockMessage(sig)
        }

      case GetBlockMessage(signature) =>
        Controller.blockchainStorage.blockByHeader(signature) match {
          case Some(block) => self ! BlockMessage(block.height().get, block)
          case None => self ! Blacklist
        }

      case BlockMessage(height, block) =>
        require(block != null)
        log.info(s"Got block, height $height , local height: " + Controller.blockchainStorage.height())

        if (height == Controller.blockchainStorage.height() + 1) {
          if (Block.isNewBlockValid(block)) {
            networkController ! NewBlock(block, Some(remote))
          } else {
            log.info(s"Got non-valid block (height of a block: $height")
          }
        }

      case TransactionMessage(transaction) =>
        if (!transaction.isSignatureValid || transaction.transactionType == TransactionType.GenesisTransaction) {
          self ! Blacklist
        } else if (transaction.hasMinimumFee && transaction.hasMinimumFeePerByte) {
          Controller.onNewOffchainTransaction(transaction)
        }

      case a: Any => log.warn(s"PeerConnectionHandler: got something strange $a")
    }
  }

  override def receive = {
    case PingRemote => self ! PingMessage

    case SendBlockchainScore =>
      self ! ScoreMessage(Controller.blockchainStorage.height(), Controller.blockchainStorage.score)

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
        self ! ByteString(msg.serialize())
        flags = newFlags
      }

    case data: ByteString =>
      connection ! Write(data)

    case CommandFailed(w: Write) =>
      log.info(s"Write failed : $w " + remote)
      PeerManager.blacklistPeer(remote)
      connection ! Close

    case Received(data) =>
      Message.parse(data.toByteBuffer) match {
        case Success(message) =>
          log.info("received message " + message.getClass.getSimpleName + " from " + remote)
          handleMessage(message)

        case Failure(e) =>
          log.info(s"Corrupted data from: " + remote + " : " + e.getMessage)
          connection ! Close
        //context stop self
      }

    case cc: ConnectionClosed =>
      networkController ! NetworkController.PeerDisconnected(remote)
      log.info("Connection closed to : " + remote + ": " + cc.getErrorCause)

    case CloseConnection =>
      log.info(s"Enforced to abort communication with: " + remote)
      connection ! Close

    case Blacklist =>
      log.info(s"Going to blacklist " + remote)
    //todo: real blacklisting
    //  PeerManager.blacklistPeer(remote)
    //  connection ! Close

    case BestPeer(peer, betterThanLocal) => best = betterThanLocal && (peer == remote)

    case nonsense: Any => log.warn(s"Strange input: $nonsense")
  }
}

object PeerConnectionHandler {

  case object PingRemote

  case object SendBlockchainScore

  case object CloseConnection

  case object Blacklist

  case class BestPeer(remote: InetSocketAddress, betterThanLocal: Boolean)
}