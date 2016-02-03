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

  //todo: change all usage scenarios to avoid unclear forSelf argument value choice
  private def knownPeers(forSelf: Boolean): Seq[InetSocketAddress] = {
    val dbPeers = PeerDatabaseImpl.knownPeers(forSelf)
    log.info("Peers retrieved from database : " + dbPeers)
    if (dbPeers.size < DatabasePeersAmount) {
      val allPeers = settings.knownPeers ++ dbPeers
      log.info("Peers retrieved including settings : " + allPeers)
      allPeers
    } else dbPeers
  }

  private def randomPeer(): Option[InetSocketAddress] = {
    val peers = knownPeers(true)
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  //todo: combine AddKnownPeer & UpdatePeer?
  private def peerLists: Receive = {
    case AddKnownPeer(address) =>
      val peerInfo = PeerInfo(System.currentTimeMillis())
      PeerDatabaseImpl.addOrUpdateKnownPeer(address, peerInfo)

    case UpdatePeer(address, isSelf) =>
      val peerInfo = PeerInfo(System.currentTimeMillis(), isSelf)
      PeerDatabaseImpl.addOrUpdateKnownPeer(address, peerInfo)

    case KnownPeers =>
      sender() ! knownPeers(false)

    case RandomPeer =>
      sender() ! randomPeer()

    case RandomPeers(howMany: Int) =>
      sender() ! Random.shuffle(knownPeers(false)).take(3)

    case FilterPeers(sendingStrategy: SendingStrategy) =>
      sender() ! sendingStrategy.choose(connectedPeers.keys.toSeq)
  }

  private def apiInterface: Receive = {
    case GetConnectedPeers =>
      sender() ! (connectedPeers.values.flatten.toSeq: Seq[Handshake])

    case GetAllPeers =>
      sender() ! knownPeers(true)
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

      if (toUpdate.isEmpty) {
        log.error("No peer to update")
      } else {
        val newCp = toUpdate
          .find(t => handshake.declaredAddress.contains(t._1.socketAddress))
          .getOrElse(toUpdate.head)
          ._1

        toUpdate.keys.foreach(connectedPeers.remove)

        //drop connection to self if occurred
        if (handshake.nodeNonce == application.settings.nodeNonce) {
          newCp.handlerRef ! PeerConnectionHandler.CloseConnection
          self ! UpdatePeer(handshake.declaredAddress.getOrElse(address), self = true)
        } else {
          handshake.declaredAddress.foreach(address => self ! PeerManager.AddKnownPeer(address))
          connectedPeers += newCp -> Some(handshake)
        }
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

  case class UpdatePeer(address: InetSocketAddress, self: Boolean)

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
