package scorex.network.peer

import java.net.InetSocketAddress

import akka.actor.Actor
import scorex.app.Application
import scorex.network._
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.util.Random

/**
  * Must be singleton
  *
  * @param application - Scorex-based application
  */
class PeerManager(application: Application) extends Actor with ScorexLogging {

  import PeerManager._

  private val connectedPeers = mutable.Map[ConnectedPeer, Option[Handshake]]()
  private var connectingPeer: Option[InetSocketAddress] = None

  private lazy val settings = application.settings
  private lazy val networkController = application.networkController

  //TODO Option[String]
  private lazy val peerDatabase = new PeerDatabaseImpl(settings, settings.dataDirOpt.map(f => f + "/peers.dat"))

  settings.knownPeers.foreach { address =>
    val defaultPeerInfo = PeerInfo(System.currentTimeMillis(), None, None)
    peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
  }

  private def randomPeer(): Option[InetSocketAddress] = {
    val peers = peerDatabase.knownPeers(true).keys.toSeq
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  private def peerListOperations: Receive = {
    case AddOrUpdatePeer(address, peerNonceOpt, peerNameOpt) =>
      val peerInfo = PeerInfo(System.currentTimeMillis(), peerNonceOpt, peerNameOpt)
      peerDatabase.addOrUpdateKnownPeer(address, peerInfo)

    case KnownPeers =>
      sender() ! peerDatabase.knownPeers(false).keys.toSeq

    case RandomPeer =>
      sender() ! randomPeer()

    case RandomPeers(howMany: Int) =>
      sender() ! Random.shuffle(peerDatabase.knownPeers(false).keys.toSeq).take(howMany)

    case FilterPeers(sendingStrategy: SendingStrategy) =>
      sender() ! sendingStrategy.choose(connectedPeers.keys.toSeq)
  }

  private def apiInterface: Receive = {
    case GetConnectedPeers =>
      sender() ! (connectedPeers.values.flatten.toSeq: Seq[Handshake])

    case GetAllPeers =>
      sender() ! peerDatabase.knownPeers(true)

    case GetBlacklistedPeers =>
      sender() ! peerDatabase.blacklistedPeers()
  }

  private def peerCycle: Receive = {
    case Connected(newPeer@ConnectedPeer(remote, _)) =>
      if (peerDatabase.isBlacklisted(newPeer.socketAddress)) {
        log.info(s"Got incoming connection from blacklisted $remote")
      } else {
        connectedPeers += newPeer -> None
        if (connectingPeer.contains(remote)) {
          log.info(s"Connected to $remote")
          connectingPeer = None
        } else {
          log.info(s"Got incoming connection from $remote")
        }
      }

    case Handshaked(address, handshake) =>
      if (peerDatabase.isBlacklisted(address)) {
        log.info(s"Got handshake from blacklisted $address")
      } else {
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
          } else {
            handshake.declaredAddress.foreach(address => self ! PeerManager.AddOrUpdatePeer(address, None, None))
            connectedPeers += newCp -> Some(handshake)
          }
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

    case AddToBlacklist(peer) =>
      log.info(s"Blacklist peer $peer")
      peerDatabase.blacklistPeer(peer)

    case RemoveFromBlacklist(peer) =>
      log.info(s"Remove peer $peer from blacklist")
      peerDatabase.removeFromBlacklist(peer)
  }: Receive) orElse peerListOperations orElse apiInterface orElse peerCycle
}

object PeerManager {

  case class AddOrUpdatePeer(address: InetSocketAddress, peerNonce: Option[Long], peerName: Option[String])

  case object KnownPeers

  case object RandomPeer

  case class RandomPeers(hawMany: Int)

  case object CheckPeers

  case class Connected(newPeer: ConnectedPeer)

  case class Handshaked(address: InetSocketAddress, handshake: Handshake)

  case class Disconnected(remote: InetSocketAddress)

  case class AddToBlacklist(remote: InetSocketAddress)

  case class RemoveFromBlacklist(remote: InetSocketAddress)

  case class FilterPeers(sendingStrategy: SendingStrategy)

  case object GetAllPeers

  case object GetBlacklistedPeers

  case object GetConnectedPeers

}
