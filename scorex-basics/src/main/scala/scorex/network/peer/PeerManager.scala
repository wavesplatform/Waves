package scorex.network.peer

import java.net.InetSocketAddress

import akka.actor.Actor
import scorex.app.Application
import scorex.network._
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.util.Random


class PeerManager(application: Application) extends Actor with ScorexLogging {

  import PeerManager._

  private val DatabasePeersAmount = 1000

  private val connectedPeers = mutable.Map[ConnectedPeer, Option[Handshake]]()
  private var connectingPeer: Option[InetSocketAddress] = None

  private lazy val settings = application.settings
  private lazy val networkController = application.networkController

  private def knownPeers(): Seq[InetSocketAddress] = {
    val dbPeers = PeerDatabaseImpl.knownPeers()
    log.info("Peers retrieved from database : " + dbPeers)
    if (dbPeers.size < DatabasePeersAmount) {
      val allPeers = settings.knownPeers ++ dbPeers
      log.info("Peers retrieved including settings : " + allPeers)
      allPeers
    } else dbPeers
  }

  private def randomPeer(): Option[InetSocketAddress] = {
    val peers = knownPeers()
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  private def peerLists: Receive = {
    case AddKnownPeer(address) =>
      if (!knownPeers().contains(address)) PeerDatabaseImpl.addKnownPeer(address)

    case KnownPeers => sender() ! knownPeers()

    case RandomPeer =>
      sender() ! randomPeer()

    case RandomPeers(howMany: Int) =>
      sender() ! Random.shuffle(knownPeers()).take(3)

    case FilterPeers(sendingStrategy: SendingStrategy) =>
      sender() ! sendingStrategy.choose(connectedPeers.keys.toSeq)
  }

  private def apiInterface: Receive = {
    case GetConnectedPeers =>
      sender() ! (connectedPeers.values.flatten.toSeq: Seq[Handshake])

    case GetAllPeers =>
      sender() ! knownPeers()
  }

  private def peerCycle: Receive = {
    case Connected(newPeer@ConnectedPeer(remote, _)) =>
      connectedPeers += newPeer -> None
      if (connectingPeer.contains(remote)) {
        log.info(s"Connected to $remote")
        connectingPeer = None
      } else {
        log.info(s"Got incoming connection from $remote")
      }

    case Handshaked(address, handshake) =>
      val toUpdate = connectedPeers.filter { case (cp, h) =>
        cp.socketAddress == address || h.map(_.nodeNonce == handshake.nodeNonce).getOrElse(true)
      }

      val newCp = toUpdate.find(_._1.socketAddress.getAddress.toString == handshake.declaredAddress)
        .getOrElse(toUpdate.head)._1

      toUpdate.keys.foreach(connectedPeers.remove)

      //drop connection to self if occurred
      if (handshake.nodeNonce == application.settings.nodeNonce) {
        newCp.handlerRef ! PeerConnectionHandler.CloseConnection
      } else {
        handshake.declaredAddress.foreach(address => self ! PeerManager.AddKnownPeer(address))
        connectedPeers += newCp -> Some(handshake)
      }

    case Disconnected(remote) =>
      connectedPeers.retain { case (p, _) => p.socketAddress != remote }
      if (connectingPeer.contains(remote)) {
        connectingPeer = None
      }
  }

  override def receive: Receive = ({
    case CheckPeers =>
      if (connectedPeers.size < settings.maxConnections && connectingPeer.isEmpty) {
        randomPeer().foreach { address =>
          if (!connectedPeers.map(_._1.socketAddress).contains(address)) {
            connectingPeer = Some(address)
            networkController ! NetworkController.ConnectTo(address)
          }
        }
      }
  }: Receive) orElse peerLists orElse apiInterface orElse peerCycle
}

object PeerManager {

  case class AddKnownPeer(address: InetSocketAddress)

  case object KnownPeers

  case object RandomPeer

  case class RandomPeers(hawMany: Int)

  case object CheckPeers

  case class Connected(newPeer: ConnectedPeer)

  case class Handshaked(address: InetSocketAddress, handshake: Handshake)

  case class Disconnected(remote: InetSocketAddress)

  case class FilterPeers(sendingStrategy: SendingStrategy)

  case object GetAllPeers

  case object GetConnectedPeers

}
