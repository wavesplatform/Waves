package scorex.network.peer

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import scorex.app.Application
import scorex.network.NetworkController.{SendToNetwork, ShutdownNetwork}
import scorex.network._
import scorex.network.message.{Message, MessageSpec}
import scorex.network.message.MessageHandler.RawNetworkData
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

  private implicit val system = context.system

  import PeerManager._
  import PeerConnectionHandler._

  private val connectedPeers = mutable.Map[InetSocketAddress, PeerConnection]()
  private var connectingPeer: Option[InetSocketAddress] = None

  private val nonces = new mutable.HashMap[Long, MutableSet[InetSocketAddress]] with mutable.MultiMap[Long, InetSocketAddress]
  nonces += application.settings.nodeNonce -> MutableSet.empty

  assert(nonces.keys.size == 1, "app nonce should be registered on start")

  private lazy val settings = application.settings
  private lazy val networkController = application.networkController

  private lazy val peerDatabase = new PeerDatabaseImpl(settings, settings.dataDirOpt.map(f => f + "/peers.dat"))

  settings.knownPeers.foreach { address =>
    val defaultPeerInfo = PeerInfo(System.currentTimeMillis(), None, None)
    peerDatabase.addOrUpdateKnownPeer(address, defaultPeerInfo)
  }

  private def peerCycle: Receive = {
    case Connected(remote, handlerRef) =>
      if (peerDatabase.isBlacklisted(remote)) {
        log.info(s"Got incoming connection from blacklisted $remote")
      } else {
        connectedPeers += remote -> PeerConnection(handlerRef, None)
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

    case RawNetworkData(spec, msgData, remote) => processDataFromNetwork(spec, msgData, remote)

    case SendToNetwork(message, sendingStrategy) => sendDataToNetwork(message, sendingStrategy)

    case Disconnected(remote) => disconnect(remote)
  }

  private def peerListOperations: Receive = {
    case AddOrUpdatePeer(address, peerNonceOpt, peerNameOpt) =>
      addOrUpdatePeer(address, peerNonceOpt, peerNameOpt)

    case GetRandomPeers(howMany, excludeSelf) =>
      sender() ! Random.shuffle(peerDatabase.knownPeers(excludeSelf).keys.toSeq).take(howMany)

    case GetConnectedPeers =>
      val peers = connectedPeers.filter(_._2.handshake.isDefined).map { case (k, v) => (k, v.handshake.get)}.toList
      sender() ! peers

    case GetAllPeers =>
      sender() ! peerDatabase.knownPeers(true)

    case GetBlacklistedPeers =>
      sender() ! peerDatabase.blacklisted
  }

  private def randomPeer(): Option[(InetSocketAddress, PeerInfo)] = {
    val peers = peerDatabase.knownPeers(true).toSeq
    if (peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
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
    connectedPeers.get(remote).flatMap(_.handshake)
      .orElse({log.warn(s"No connected peer matches $remote"); None })
      .foreach { handshakeData =>
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
      .foreach { nonce =>
        nonces(nonce).foreach(connectedPeers.remove)
        nonces -= nonce
      }
  }

  private def handleHandshake(address: InetSocketAddress, handshake: Handshake): Unit =
    connectedPeers.get(address).filter(_.handshake.isEmpty)
      .orElse ( { log.error("No peer to validate"); None } )
      .foreach { case c @ PeerConnection(_, _) =>

        val handshakeNonce = handshake.nodeNonce

        def updateHandshakedPeer() = nonces.addBinding(handshakeNonce, address)

        if (nonces.keys.contains(handshakeNonce)) {
          val peers = nonces(handshakeNonce)

          if (peers.nonEmpty)
            log.info(s"Peer $address has come with nonce $handshakeNonce corresponding to: ${peers.mkString(",")}")
          else
            log.info("Drop connection to self")

          updateHandshakedPeer()
          connectedPeers -= address
          c.handlerRef ! CloseConnection
        } else {
          updateHandshakedPeer()
          handshake.declaredAddress.foreach { addOrUpdatePeer(_, None, None) }
          connectedPeers += address -> c.copy(handshake = Some(handshake))
        }
      }

  private def addOrUpdatePeer(address: InetSocketAddress, peerNonce: Option[Long], nodeName: Option[String]) =
    if (application.settings.acceptExternalPeerData) {
      peerDatabase.addOrUpdateKnownPeer(address, PeerInfo(System.currentTimeMillis(), peerNonce, nodeName))
    }

  private def blackListOperations: Receive = {
    case AddToBlacklist(nodeNonce, address) =>
      nonces.getOrElse(nodeNonce, MutableSet(address)).foreach {
        addr =>
          log.info(s"Blacklist peer $addr")
          peerDatabase.blacklist(addr)
          connectedPeers.get(addr).foreach(_.handlerRef ! CloseConnection)
      }
  }

  override def receive: Receive = ({
    case CheckPeers =>
      if (connectedPeers.size < settings.maxConnections && connectingPeer.isEmpty) {
        randomPeer().foreach { case (address, _) =>
          if ( ! (nonces.values.flatten.contains(address) || connectedPeers.keys.contains(address)) ) {
            log.debug(s"Trying connect to random peer $address")
            connectingPeer = Some(address)
            networkController ! NetworkController.ConnectTo(address)
          }
        }
      }

    case ShutdownNetwork => connectedPeers.values.foreach(_.handlerRef ! CloseConnection)

  }: Receive) orElse blackListOperations orElse peerListOperations orElse peerCycle
}

object PeerManager {

  case class AddOrUpdatePeer(address: InetSocketAddress, peerNonce: Option[Long], peerName: Option[String])

  case object CheckPeers

  case class Connected(socketAddress: InetSocketAddress, handlerRef: ActorRef)

  case class Handshaked(address: InetSocketAddress, handshake: Handshake)

  case class Disconnected(remote: InetSocketAddress)

  case class AddToBlacklist(nodeNonce: Long, address: InetSocketAddress)

  case object GetAllPeers

  case class GetRandomPeers(howMany: Int, excludeSelf: Boolean = true)

  case object GetBlacklistedPeers

  case object GetConnectedPeers

  case class PeerConnection(handlerRef: ActorRef, handshake: Option[Handshake])
}
