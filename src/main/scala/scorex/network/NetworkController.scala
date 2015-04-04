package scorex.network

import java.net.InetSocketAddress
import java.util.logging.Logger
import akka.actor.{ActorRef, Props, Actor}
import akka.io.Tcp._
import akka.io.{Tcp, IO}
import scorex.database.PrunableBlockchainStorage
import scorex.block.{NewBlock, BlockchainController}
import BlockchainController.GetMaxChainScore
import scorex.network.message.Message
import scorex.network.message._
import settings.Settings
import scala.collection.mutable
import scala.util.{Try, Random}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class NetworkController extends Actor {

  import NetworkController._

  private implicit val system = context.system

  //todo: longest chain rule doesn't make sense for proof-of-stake at all and probably dangerous!
  //todo: so should be replaced with cumulative difficulty (aka maxvalid function)
  private val connectedPeers = mutable.Map[InetSocketAddress, PeerData]()

  private def maxPeerHeight() = Try(connectedPeers.maxBy(_._2.height)._2.height).toOption

  private def maxHeightHandler() = Try(connectedPeers.maxBy(_._2.height)._2.handler).toOption

  //todo: a bit stupid workaround, consider more elegant solution for circular linking
  private var blockchainControllerOpt: Option[ActorRef] = None

  IO(Tcp) ! Bind(self, new InetSocketAddress("localhost", Settings.Port))

  override def receive = {
    case b@Bound(localAddress) =>
      Logger.getGlobal.info("Successfully bound to the port " + Settings.Port)
      context.system.scheduler.schedule(200.millis, 3.seconds)(self ! CheckPeers)
      context.system.scheduler.schedule(500.millis, 5.seconds)(self ! AskForPeers)

    case CommandFailed(_: Bind) =>
      Logger.getGlobal.warning("Network port " + Settings.Port + " already in use!")
      System.exit(10) //todo: too rough way to shutdown?
      context stop self

    case CheckPeers =>
      if (connectedPeers.size < Settings.maxConnections) {
        val peer = PeerManager.randomPeer()
        if (!connectedPeers.contains(peer)) {
          IO(Tcp) ! Connect(peer) //todo: connection timeout
        }
      }

    case AskForPeers =>
      val handlers = connectedPeers.values.toList
      if (handlers.nonEmpty) {
        val randomHandler = handlers(Random.nextInt(handlers.size)).handler
        randomHandler ! GetPeersMessage(mbId = Some(Random.nextInt(5000000)))
      }

    case c@Connected(remote, local) if remote.getPort == Settings.Port =>
      Logger.getGlobal.info(s"Connected to $remote")
      val connection = sender()
      val handler = context.actorOf(Props(classOf[PeerConnectionHandler], self, connection, remote))
      connection ! Register(handler)    //todo: keepOpenOnPeerClosed param?
      connectedPeers += remote -> PeerData(handler, 1)
      PeerManager.peerConnected(remote)

    case CommandFailed(c: Connect) =>
      Logger.getGlobal.info("Failed to connect to : " + c.remoteAddress)
      connectedPeers -= c.remoteAddress
      PeerManager.peerDisconnected(c.remoteAddress)

    case CommandFailed(cmd: Tcp.Command) =>
      Logger.getGlobal.info("Failed to execute command : " + cmd)

    case ShutdownNetwork =>
      Logger.getGlobal.info("Going to shutdown all connections & unbind port")
      connectedPeers.values.foreach(_.handler ! PeerConnectionHandler.CloseConnection)
      self ! Unbind
      context stop self

    case PeerDisconnected(remote) =>
      connectedPeers -= remote
      PeerManager.peerDisconnected(remote)

    case BroadcastMessage(message, exceptOf) =>
      Logger.getGlobal.info(s"Broadcasting message $message")
      connectedPeers.foreach { case (remote, PeerData(handler, _)) =>
        if (!exceptOf.contains(remote)) handler ! message
      }
      Logger.getGlobal.info("Broadcasting end")

    case SendMessageToBestPeer(message) =>
      maxHeightHandler().map { handler =>
        Logger.getGlobal.info(s"Sending $message to a best peer")
        handler ! message
      }

    case GetPeers => sender() ! connectedPeers.toMap

    case GetMaxChainScore =>
      if(blockchainControllerOpt.isEmpty) blockchainControllerOpt = Some(sender())
      sender() ! BlockchainController.MaxChainScore(maxPeerHeight())

    case NewBlock(block, Some(sndr)) =>
      blockchainControllerOpt.map { blockchainController =>
        blockchainController ! NewBlock(block, Some(sndr))
        val height = PrunableBlockchainStorage.height()
        self ! BroadcastMessage(BlockMessage(height, block), List(sndr))
      }

    case a:Any => Logger.getGlobal.warning(s"NetworkController: got something strange $a")
  }
}

object NetworkController {

  private case object CheckPeers

  private case object AskForPeers

  case object ShutdownNetwork

  case object GetPeers

  case object GetMaxHeight

  case class PeerData(handler: ActorRef, height: Int)

  //todo: add ping value?

  case class PeerDisconnected(address: InetSocketAddress)

  case class UpdateHeight(remote: InetSocketAddress, height: Int)

  case class SendMessageToBestPeer(message: Message)

  case class BroadcastMessage(message: Message, exceptOf: List[InetSocketAddress] = List())

}


