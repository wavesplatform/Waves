package scorex.network.peer

import java.net.InetSocketAddress

import akka.actor.Actor
import scorex.app.Application
import scorex.network._
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.{Set => MutableSet}
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

  private val nonces = new mutable.HashMap[Long, MutableSet[InetSocketAddress]] with mutable.MultiMap[Long, InetSocketAddress]
  nonces += application.settings.nodeNonce -> MutableSet.empty

  assert(nonces.keys.size == 1, "app nonce should be registered on start")

  private lazy val settings = application.settings
  private lazy val networkController = application.networkController

  //TODO Option[String]
  private lazy val peerDatabase = new PeerDatabaseImpl(settings, settings.dataDirOpt.map(f => f + "/peers.dat"))

  settings.knownPeers.foreach { address =>
    val defaultPeerInfo = PeerInfo(System.currentTimeMillis(), None, None)
    peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
  }

  private def randomPeer(): Option[(InetSocketAddress, PeerInfo)] = {
    val peers = peerDatabase.knownPeers(true).toSeq
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  private def peerListOperations: Receive = {
    case AddOrUpdatePeer(address, peerNonceOpt, peerNameOpt) =>
      addOrUpdatePeer(address, peerNonceOpt, peerNameOpt)

    case KnownPeers =>
      sender() ! peerDatabase.knownPeers(false).keys.toSeq

    case RandomPeers(howMany, excludeSelf) =>
      sender() ! Random.shuffle(peerDatabase.knownPeers(excludeSelf).keys.toSeq).take(howMany)

    case FilterPeers(sendingStrategy: SendingStrategy) =>
      val chosen = sendingStrategy.choose(connectedPeers.keys.toSeq)
      log.trace(s"${chosen.length} peers have been chosen among ${connectedPeers.size}")
      sender() ! chosen
  }

  private def apiInterface: Receive = {
    case GetConnectedPeers =>
      val peers = connectedPeers.filter(_._2.isDefined).map { case (k, v) => (k.socketAddress, v.get)}.toList
      sender() ! peers

    case GetAllPeers =>
      sender() ! peerDatabase.knownPeers(true)

    case GetBlacklistedPeers =>
      sender() ! peerDatabase.blacklisted
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
        handleHandshake(address, handshake)
      }

    case Disconnected(remote) =>
      if (connectingPeer.contains(remote)) {
        connectingPeer = None
      }
      connectedPeers.find(_._1.socketAddress == remote).flatMap(_._2).map(_.nodeNonce)
        .foreach {
          nonce =>
            val peersWithTheNonce = nonces(nonce)
            connectedPeers.retain { case (p, _) => ! peersWithTheNonce.contains(p.socketAddress) }
            nonces -= nonce
        }
 }

  private def handleHandshake(address: InetSocketAddress, handshake: Handshake): Unit =
    connectedPeers.find(peer => peer._1.socketAddress == address && peer._2.isEmpty)
      .orElse ( { log.error("No peer to validate"); None } )
      .foreach { case (connectedPeer, _) =>

        val handshakeNonce = handshake.nodeNonce

        def updateHandshakedPeer() = nonces.addBinding(handshakeNonce, address)

        if (nonces.keys.contains(handshakeNonce)) {
          val peers = nonces(handshakeNonce)

          if (peers.nonEmpty)
            log.info(s"Peer $address has come with nonce $handshakeNonce corresponding to: ${peers.mkString(",")}")
          else
            log.info("Drop connection to self")

          updateHandshakedPeer()
          connectedPeers -= connectedPeer
          connectedPeer.handlerRef ! PeerConnectionHandler.CloseConnection
        } else {
          updateHandshakedPeer()
          handshake.declaredAddress.foreach { addOrUpdatePeer(_, None, None) }
          connectedPeers += connectedPeer -> Some(handshake)
        }
      }

  private def addOrUpdatePeer(address: InetSocketAddress, peerNonce: Option[Long], nodeName: Option[String]) =
    if (application.settings.acceptExternalPeerData) {
      peerDatabase.addOrUpdateKnownPeer(address, PeerInfo(System.currentTimeMillis(), peerNonce, nodeName))
    }

  private def blackListOperations: Receive = {
    case AddToBlacklist(peer) =>
      log.info(s"Blacklist peer $peer")
      peerDatabase.blacklist(peer)

    case RemoveFromBlacklist(peer) =>
      log.info(s"Remove peer $peer from blacklist")
      peerDatabase.unBlacklist(peer)
  }

  override def receive: Receive = ({
    case CheckPeers =>
      if (connectedPeers.size < settings.maxConnections && connectingPeer.isEmpty) {
        randomPeer().foreach { case (address, _) =>
          val addresses = connectedPeers.map(_._1.socketAddress)
          if ( ! (nonces.values.flatten.contains(address) || addresses.contains(address)) ) {
            log.debug(s"Trying connect to random peer $address")
            connectingPeer = Some(address)
            networkController ! NetworkController.ConnectTo(address)
          }
        }
      }
  }: Receive) orElse blackListOperations orElse peerListOperations orElse apiInterface orElse peerCycle

}

object PeerManager {

  case class AddOrUpdatePeer(address: InetSocketAddress, peerNonce: Option[Long], peerName: Option[String])

  case object KnownPeers

  case class RandomPeers(howMany: Int, excludeSelf: Boolean = true)

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
