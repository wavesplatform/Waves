package scorex.network

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef, Props}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import scorex.app.Application
import scorex.network.message.BasicMessagesRepo
import scorex.network.peer.PeerManager
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Random, Try}


//must be singleton
class NetworkController(application: Application) extends Actor with ScorexLogging {

  import NetworkController._

  private implicit val system = context.system

  private val connectedPeers = mutable.Map[InetSocketAddress, PeerData]()
  private val connectingPeers = mutable.Buffer[InetSocketAddress]()

  private def maxPeerScore() = Try(connectedPeers.maxBy(_._2.blockchainScore)._2.blockchainScore).toOption.flatten

  private def maxScoreHandler() = Try(connectedPeers.maxBy(_._2.blockchainScore)._2.handler).toOption

  IO(Tcp) ! Bind(self, new InetSocketAddress(InetAddress.getByName(settings.bindAddress), settings.Port))

  private def updateScore(remote: InetSocketAddress, height: Int, score: BigInt) = {
    val prevBestScore = maxPeerScore().getOrElse(0: BigInt)

    connectedPeers.get(remote).foreach { peerData =>
      connectedPeers.put(remote, peerData.copy(blockchainScore = Some(score)))
      log.info(s"Score updated for $remote: h. $height -- $score")
    }

    if (score > prevBestScore) {
      connectedPeers.foreach { case (_, PeerData(handler, _)) =>
        handler ! PeerConnectionHandler.BestPeer(remote, score > transModule.history.score)
      }
    }
  }

  override def receive = {
    case b@Bound(localAddress) =>
      log.info("Successfully bound to the port " + settings.Port)
      context.system.scheduler.schedule(200.millis, 3.seconds)(self ! CheckPeers)
      context.system.scheduler.schedule(1500.millis, 10.seconds)(self ! AskForPeers)

    case CommandFailed(_: Bind) =>
      log.error("Network port " + settings.Port + " already in use!")
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

    case c@Connected(remote, local) =>
      log.info(s"Connected to $remote")
      connectingPeers -= remote
      val connection = sender()
      val handler = context.actorOf(Props(classOf[PeerConnectionHandler], connection, remote))
      connection ! Register(handler)
      connectedPeers += remote -> PeerData(handler, None)
      peerManager.peerConnected(remote)

    case CommandFailed(c: Connect) =>
      log.info("Failed to connect to : " + c.remoteAddress)
      connectedPeers -= c.remoteAddress
      peerManager.peerDisconnected(c.remoteAddress)

    case CommandFailed(cmd: Tcp.Command) =>
      log.info("Failed to execute command : " + cmd)

    case ShutdownNetwork =>
      log.info("Going to shutdown all connections & unbind port")
      connectedPeers.values.foreach(_.handler ! PeerConnectionHandler.CloseConnection)
      self ! Unbind
      context stop self

    case PeerDisconnected(remote) =>
      connectedPeers -= remote
      peerManager.peerDisconnected(remote)

    case AskForPeers =>
      self ! SendMessageToRandomPeer(BasicMessagesRepo.GetPeersMessage)


    case BroadcastMessage(message, exceptOf) =>
      log.info(s"Broadcasting message $message")
      connectedPeers.foreach { case (remote, PeerData(handler, _)) =>
        if (!exceptOf.contains(remote)) handler ! message
      }
      log.info("Broadcasting end")

    case SendMessageToBestPeer(message) =>
      maxScoreHandler().foreach { handler =>
        log.info(s"Sending $message to a best peer ${handler.path}")
        handler ! message
      }

    case SendMessageToRandomPeer(message) =>
      val handlers = connectedPeers.values.toList
      if (handlers.nonEmpty) {
        val randomHandler = handlers(Random.nextInt(handlers.size)).handler
        randomHandler ! message
      }

    case GetPeers => sender() ! connectedPeers.toMap

    case GetMaxChainScore =>
      sender() ! BlockchainSyncer.MaxChainScore(maxPeerScore())

    case NewBlock(block, Some(sndr)) =>
      application.blockchainSyncer ! NewBlock(block, Some(sndr))
      self ! BroadcastMessage(BlockMessage(block), List(sndr))

    case UpdateBlockchainScore(remote, height, score) => updateScore(remote, height, score)

    case nonsense: Any => log.warn(s"NetworkController: got something strange $nonsense")
  }
}

case class PeerData(peer: PeerConnectionHandler, blockchainScore: Option[BigInt])

object NetworkController {

  private case object CheckPeers

  private case object AskForPeers

  case object ShutdownNetwork

  case object GetPeers

  case object GetMaxBlockchainScore

  case class PeerDisconnected(address: InetSocketAddress)

  case class UpdateBlockchainScore(remote: InetSocketAddress, height: Int, score: BigInt)

  case class SendMessageToBestPeer(msg: message.Message[_])

  case class SendMessageToRandomPeer(msg: message.Message[_])

  case class BroadcastMessage(msg: message.Message[_], exceptOf: Seq[InetSocketAddress] = List())
}



case class Rule(sendingStrategy: SendingStrategy,
                interaction: Interaction,
                scheduler: Option[(FiniteDuration, FiniteDuration)]) //initial delay, delay

//get peers

trait NetworkApplicationLogic {

  val peerManager: PeerManager

  val peers: Seq[PeerConnectionHandler]

  object peersExchange extends Rule(SendToRandom, PeersInteraction(peerManager), Some(1.second -> 3.seconds))

  val rules: Seq[Rule] = Seq(peersExchange)
}


trait BlockchainApplicationLogic extends NetworkApplicationLogic {

  object newBlock extends Rule(Broadcast, BlockInteraction, None)

  //object bestExtension extends Rule(BestPeer, SignaturesInteraction, None)

  override val rules = super.rules ++ Seq()
}

