package scorex.network.peer

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import com.google.common.collect.HashMultimap
import scorex.app.Application
import scorex.network.NetworkController.{SendToNetwork, ShutdownNetwork}
import scorex.network._
import scorex.network.message.MessageHandler.RawNetworkData
import scorex.network.message.{Message, MessageSpec}
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

/**
  * Must be singleton
  *
  * @param application - Scorex-based application
  */
class PeerManager(application: Application) extends Actor with ScorexLogging {

  private implicit val system = context.system

  import PeerConnectionHandler._
  import PeerManager._

  private val connectedPeers = mutable.Map[InetSocketAddress, PeerConnection]()
  private var connectingPeer: Option[InetSocketAddress] = None

  private val nonces = HashMultimap.create[Long, InetSocketAddress]
  nonces.put(application.settings.nodeNonce, new InetSocketAddress("self", application.settings.port))

  private lazy val settings = application.settings
  private lazy val networkController = application.networkController

  private val peerDatabase = new PeerDatabaseImpl(settings, settings.dataDirOpt.map(f => f + "/peers.dat"))

  private val visitPeersInterval = application.settings.peersDataResidenceTime / 10
  context.system.scheduler.schedule(visitPeersInterval, visitPeersInterval, self, MarkConnectedPeersVisited)

  settings.knownPeers.foreach {
    peerDatabase.mergePeerInfo(_, PeerInfo())
  }

  private val blacklistListeners: scala.collection.mutable.Set[ActorRef] = scala.collection.mutable.Set.empty[ActorRef]

  private def peerCycle: Receive = {
    case Connected(remote, handlerRef, ownSocketAddress, inbound) =>

      val connectionsCount = connectedPeers.count(_._2.inbound == inbound)

      if (isBlacklisted(remote)) {
        log.warn(s"Got incoming connection from blacklisted $remote")
        handlerRef ! CloseConnection
      } else if (connectionsCount >= settings.maxConnections && connectingPeer != Option(remote)) {
        log.info(s"Number of connections exceeded ${settings.maxConnections}, disconnect $remote")
        handlerRef ! CloseConnection
      } else {
        handleNewConnection(remote, handlerRef, ownSocketAddress, inbound)
      }

    case Handshaked(address, handshake) =>
      if (isBlacklisted(address)) {
        log.warn(s"Got handshake from blacklisted $address")
        connectedPeers.get(address).foreach(_.handlerRef ! CloseConnection)
      } else {
        handleHandshake(address, handshake)
      }

    case RawNetworkData(spec, msgData, remote) => processDataFromNetwork(spec, msgData, remote)

    case SendToNetwork(message, sendingStrategy) => sendDataToNetwork(message, sendingStrategy)

    case Disconnected(remote) => disconnect(remote)
  }

  private def isBlacklisted(address: InetSocketAddress): Boolean =
    peerDatabase.blacklistedPeers.contains(address.getHostName)

  private def getConnectedPeers = connectedPeers
    .filter(_._2.handshake.isDefined)
    .map { case (k, v) => (k, v.handshake.get) }
    .toList

  private def peerListOperations: Receive = {
    case AddOrUpdatePeer(address, peerNonceOpt, peerNameOpt) =>
      addOrUpdatePeer(address, peerNonceOpt, peerNameOpt)

    case GetConnectedPeers => sender() ! getConnectedPeers

    case GetConnectedPeersTyped => sender() ! ConnectedPeers(getConnectedPeers)

    case GetConnections => sender() ! connectedPeers.keys.toSeq

    case GetRandomPeersToBroadcast(howMany) =>
      val dbPeers = peerDatabase.knownPeers(excludeSelf = true).keySet
      val intersection = dbPeers.intersect(handshakedPeers)
      sender() ! Random.shuffle(intersection.toSeq).take(howMany)

    case GetAllPeers => sender() ! peerDatabase.knownPeers(true)

    case GetBlacklistedPeers => sender() ! peerDatabase.blacklistedPeers
  }

  private def handleNewConnection(remote: InetSocketAddress,
                                  handlerRef: ActorRef,
                                  ownSocketAddress: Option[InetSocketAddress],
                                  inbound: Boolean): Unit = {

    val connectionFromIp = connectedPeers.keys.foldLeft(0) { (c, k) =>
      if (k.getAddress.equals(remote.getAddress)) c + 1 else c
    }

    if (settings.AllowedConnectionsFromOneIPAddress > connectionFromIp) {
      val handshake = Handshake(
        application.applicationName,
        application.appVersion,
        settings.nodeName,
        application.settings.nodeNonce,
        ownSocketAddress,
        System.currentTimeMillis() / 1000)

      handlerRef ! handshake

      connectedPeers += remote -> PeerConnection(handlerRef, None, inbound)
      if (connectingPeer.contains(remote)) {
        log.info(s"Connected to $remote")
        connectingPeer = None
      } else {
        log.info(s"Got incoming connection from $remote")
      }
    } else {
      log.warn(s"Attempt to establish new connection from already connected IP address $remote")
      handlerRef ! CloseConnection
    }
  }

  private def sendDataToNetwork(message: Message[_], sendingStrategy: SendingStrategy): Unit = {
    val peers = connectedPeers
      .filter(_._2.handshake.isDefined)
      .map { case (_, c) => (c.handshake.get.nodeNonce, c.handlerRef) }
      .toSeq

    val chosen = sendingStrategy.choose(peers)
    log.trace(s"${chosen.length} peers have been chosen among ${connectedPeers.size}")
    chosen.foreach(_._2 ! message)
  }

  private def processDataFromNetwork(spec: MessageSpec[_], msgData: Array[Byte], remote: InetSocketAddress): Unit = {
    connectedPeers.get(remote) match {
      case None =>
        log.error(s"New message from unknown $remote")
        sender() ! CloseConnection

      case Some(PeerConnection(_, None, _)) =>
        log.error(s"No connected peer matches $remote")
        sender() ! CloseConnection

      case Some(PeerConnection(_, Some(handshakeData), _)) =>
        val peer = new InetAddressPeer(handshakeData.nodeNonce, remote, self)
        networkController ! Message(spec, Left(msgData), Some(peer))
    }
  }

  private def disconnect(from: InetSocketAddress): Unit = {
    if (connectingPeer.contains(from)) {
      connectingPeer = None
    }

    connectedPeers.get(from)
      .flatMap(_.handshake).map(_.nodeNonce)
      .foreach {
        nonces.removeAll(_).foreach(connectedPeers.remove)
      }

    connectedPeers.remove(from)
  }

  private def handleHandshake(address: InetSocketAddress, handshake: Handshake): Unit =
    connectedPeers.get(address) match {
      case None | Some(PeerConnection(_, Some(_), _)) =>
        log.error("No peer matching handshake")
        sender() ! CloseConnection

      case Some(connection@PeerConnection(_, None, inbound)) =>
        visit(address)

        val handshakeNonce = handshake.nodeNonce

        Option(nonces.asMap().get(handshakeNonce)) match {
          case Some(peers) =>

            if (peers.size > 1) {
              log.warn(s"Connection attempts for nonce $handshakeNonce is more than one")
              val addresses = peers.toSeq :+ address
              connectedPeers.filterKeys(addresses.contains).values.foreach {
                _.handlerRef ! CloseConnection
              }
            } else {

              if (peers.nonEmpty)
                log.info(s"Peer $address has come with nonce $handshakeNonce corresponding to: ${peers.mkString(",")}")
              else
                log.info("Drop connection to self")

              nonces.put(handshakeNonce, address)
              connectedPeers.remove(address)
              connection.handlerRef ! CloseConnection
            }

          case None =>
            nonces.put(handshakeNonce, address)
            handshake.declaredAddress.foreach {
              addOrUpdatePeer(_, None, None)
            }
            connectedPeers += address -> connection.copy(handshake = Some(handshake))
        }
    }

  private def visit(address: InetSocketAddress) = {
    peerDatabase.mergePeerInfo(address, PeerInfo(lastSeen = System.currentTimeMillis()), createIfNotExists = false)
  }

  private def addOrUpdatePeer(address: InetSocketAddress, nodeNonce: Option[Long], nodeName: Option[String]) = {
    if (application.settings.acceptExternalPeerData) {
      peerDatabase.mergePeerInfo(address, PeerInfo(nonce = nodeNonce, nodeName = nodeName))
    }
  }

  private def blacklistOperations: Receive = {
    case RegisterBlacklistListener(listener) =>
      blacklistListeners += listener
      listener ! ExistedBlacklist(peerDatabase.blacklistedPeers.toSeq)

    case UnregisterBlacklistListener(listener) =>
      blacklistListeners -= listener

    case AddToBlacklist(nodeNonce, address) =>
      (nonces.get(nodeNonce) + address).foreach {
        addr =>
          log.info(s"Blacklist peer $addr")
          peerDatabase.blacklistPeer(addr)
          connectedPeers.get(addr).foreach(_.handlerRef ! CloseConnection)

          blacklistListeners.foreach { listener =>
            listener ! BlackListUpdated(addr.getHostName)
          }
      }
  }

  private def randomPeer: Option[InetSocketAddress] = {
    val knownPeers = peerDatabase.knownPeers(true).keySet
    val candidates = knownPeers.diff(connectedPeers.keySet.union(handshakedPeers))
    if (candidates.nonEmpty) Some(candidates.toSeq(Random.nextInt(candidates.size))) else None
  }

  private def handshakedPeers = nonces.values.toSet

  override def receive: Receive = ({
    case CheckPeers =>
      if (connectedPeers.size < settings.maxConnections && connectingPeer.isEmpty) {
        randomPeer.foreach { address =>
          log.debug(s"Trying connect to random peer $address")
          connectingPeer = Some(address)
          networkController ! NetworkController.ConnectTo(address)
        }
      }

    case MarkConnectedPeersVisited => handshakedPeers.foreach(visit)

    case ShutdownNetwork => connectedPeers.values.foreach(_.handlerRef ! CloseConnection)

  }: Receive) orElse blacklistOperations orElse peerListOperations orElse peerCycle

}

object PeerManager {

  case class AddOrUpdatePeer(address: InetSocketAddress, peerNonce: Option[Long], peerName: Option[String])

  case object CheckPeers

  case class Connected(socketAddress: InetSocketAddress, handlerRef: ActorRef,
                       ownSocketAddress: Option[InetSocketAddress], inbound: Boolean)

  case class Handshaked(address: InetSocketAddress, handshake: Handshake)

  case class Disconnected(remote: InetSocketAddress)

  case class AddToBlacklist(nodeNonce: Long, address: InetSocketAddress)

  case object GetAllPeers

  case class GetRandomPeersToBroadcast(howMany: Int)

  case object GetBlacklistedPeers

  case object GetConnectedPeers

  case object GetConnectedPeersTyped

  case class ConnectedPeers(peers: Seq[(InetSocketAddress, Handshake)])

  case class PeerConnection(handlerRef: ActorRef, handshake: Option[Handshake], inbound: Boolean)

  case object GetConnections

  private case object MarkConnectedPeersVisited

  case class BlackListUpdated(host: String)

  case class RegisterBlacklistListener(listener: ActorRef)

  case class UnregisterBlacklistListener(listener: ActorRef)

  case class ExistedBlacklist(hosts: Seq[String])

}
