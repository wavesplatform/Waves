package scorex.network.peer

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.{Actor, ActorRef}
import scorex.app.Application
import scorex.network.NetworkController.SendToNetwork
import scorex.network._
import scorex.network.message.MessageHandler.RawNetworkData
import scorex.network.message.{Message, MessageSpec}
import scorex.utils.ScorexLogging

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Random, Try}

/**
  * Must be singleton
  *
  * @param application - Scorex-based application
  */
class PeerManager(application: Application) extends Actor with ScorexLogging {

  private lazy val settings = application.settings.networkSettings

  import PeerConnectionHandler._
  import PeerManager._

  private lazy val networkController = application.networkController
  val blacklistResendInterval = settings.blackListResidenceTime / 10
  private implicit val system = context.system
  private val connectedPeers = mutable.Map[InetSocketAddress, PeerConnection]()
  private val suspects = mutable.Map.empty[InetSocketAddress, Int]
  private val maybeFilename = Option(settings.file).filter(_.trim.nonEmpty)
  private val peerDatabase: PeerDatabase = new PeerDatabaseImpl(settings, maybeFilename)

  private val visitPeersInterval = settings.peersDataResidenceTime / 10
  context.system.scheduler.schedule(visitPeersInterval, visitPeersInterval, self, MarkConnectedPeersVisited)
  private val blacklistListeners: scala.collection.mutable.Set[ActorRef] = scala.collection.mutable.Set.empty[ActorRef]
  context.system.scheduler.schedule(blacklistResendInterval, blacklistResendInterval, self, BlacklistResendRequired)

  private val knownPeersAddresses = getKnownPeersAddresses(settings.knownPeers)

  knownPeersAddresses.foreach(peerDatabase.addPeer(_, Some(0), None))

  private var connectingPeer: Option[InetSocketAddress] = None

  private var maybeShutdownRequester: Option[ActorRef] = None

  private val DefaultPort = 6863

  private def getKnownPeersAddresses(knownPeers: List[String]): Seq[InetSocketAddress] = {
    Try {
      knownPeers.map { peer =>
        val addressParts = peer.split(":").map(_.trim)
        val port = if (addressParts.length == 2) addressParts(1).toInt else DefaultPort
        new InetSocketAddress(addressParts(0), port)
      }
    }.getOrElse(Seq[InetSocketAddress]())
  }

  override def receive: Receive = ({
    case CheckPeers =>
      if (connectedPeers.size < settings.maxConnections && connectingPeer.isEmpty) {
        peerDatabase.getRandomPeer(connectedPeers.keySet.toSet).foreach { address =>
          log.debug(s"Trying connect to random peer $address")
          connectingPeer = Some(address)
          networkController ! NetworkController.ConnectTo(address)
        }
      }

    case MarkConnectedPeersVisited => handshakedPeers.foreach(peerDatabase.touch)

    case CloseAllConnections =>
      if (connectedPeers.isEmpty) {
        sender() ! CloseAllConnectionsComplete
      } else {
        maybeShutdownRequester = Some(sender())
        connectedPeers.foreach(_._2.handlerRef ! CloseConnection)
      }

    case CloseConnectionCompleted(remote) =>
      disconnect(remote)
      if (connectedPeers.isEmpty) maybeShutdownRequester.foreach(_ ! CloseAllConnectionsComplete)

  }: Receive) orElse blacklistOperations orElse peerListOperations orElse peerCycle

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

    case Disconnected(remote) =>
      disconnect(remote)
  }

  private def isBlacklisted(address: InetSocketAddress): Boolean =
    peerDatabase.getBlacklist.contains(address.getHostName)

  private def getConnectedPeersWithHandshake = connectedPeers
    .filter(_._2.handshake.isDefined)
    .map { case (k, v) => (k, v.handshake.get) }
    .toList

  private def peerListOperations: Receive = {
    case AddPeer(address) =>
      peerDatabase.addPeer(address, None, None)

    case GetConnectedPeers => sender() ! getConnectedPeersWithHandshake

    case GetConnectedPeersTyped =>
      val peers = getConnectedPeersWithHandshake map {
        case (addr, h) => new InetAddressPeer(h.nodeNonce, addr, self)
      }
      sender() ! ConnectedPeers(peers.toSet)

    case GetConnections => sender() ! connectedPeers.keys.toSeq

    case GetRandomPeersToBroadcast(howMany) =>
      val dbPeers = peerDatabase.getKnownPeers.keySet
      val intersection = dbPeers.intersect(handshakedPeers)
      sender() ! Random.shuffle(intersection.toSeq).take(howMany)

    case GetAllPeers => sender() ! peerDatabase.getKnownPeers

    case GetBlacklistedPeers => sender() ! peerDatabase.getBlacklist
  }

  val AllowedConnectionsFromOneHost = 5

  private def handleNewConnection(remote: InetSocketAddress,
                                  handlerRef: ActorRef,
                                  ownSocketAddress: Option[InetSocketAddress],
                                  inbound: Boolean): Unit = {

    if (connectedPeers.contains(remote)) {
      log.debug(s"Second connection from the same InetAddress $remote is not allowed")
      handlerRef ! CloseConnection
    } else {
      val connectionFromHostCount = connectedPeers.keys.count(_.getHostName == remote.getHostName)
      if (AllowedConnectionsFromOneHost > connectionFromHostCount) {
        val handshake = Handshake(application.applicationName, application.appVersion, settings.nodeName,
          settings.nonce, ownSocketAddress, System.currentTimeMillis() / 1000)

        handlerRef ! handshake

        connectedPeers += remote -> PeerConnection(handlerRef, None, inbound)
        if (connectingPeer.contains(remote)) {
          log.info(s"Connected to $remote")
          connectingPeer = None
        } else {
          log.info(s"Got incoming connection from $remote")
        }
      } else {
        log.debug(s"Max connection from one IP exceeded $remote")
        handlerRef ! CloseConnection
      }
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

    connectedPeers.remove(from)
  }

  private def peerNonces(): Set[(InetAddress, Long)] = {
    connectedPeers.filter(_._2.handshake.isDefined).map {
      case (k, v) => (k.getAddress, v.handshake.get.nodeNonce)
    }.toSet
  }

  private def handleHandshake(address: InetSocketAddress, handshake: Handshake): Unit =
    connectedPeers.get(address) match {
      case None =>
        log.error("No peer matching handshake")
        sender() ! CloseConnection

      case Some(PeerConnection(_, Some(_), _)) =>
        log.info(s"Double handshake from $address")
        connectedPeers.remove(address)
        sender() ! CloseConnection

      case Some(connection@PeerConnection(_, None, inbound)) =>
        log.debug(s"Comparing remote application name '${handshake.applicationName}' to local '${application.applicationName}'")
        if (application.applicationName != handshake.applicationName &&
          application.applicationName.dropRight(1) != handshake.applicationName) {
          log.debug(s"Different application name: ${handshake.applicationName} from $address")
          self ! AddToBlacklist(address)
        } else if (settings.nonce == handshake.nodeNonce) {
          log.info("Drop connection to self")
          connectedPeers.remove(address)
          peerDatabase.removePeer(address)
          connection.handlerRef ! CloseConnection
        } else if (peerNonces().contains((address.getAddress, handshake.nodeNonce))) {
          log.info("Drop connection to already connected peer with the same ip and nonce")
          connectedPeers.remove(address)
          connection.handlerRef ! CloseConnection
        } else {
          val declaredAddressOption = handshake.declaredAddress
          if (!inbound) {
            log.debug(s"Got handshake on outbound connection to $address with declared address ${declaredAddressOption.getOrElse("N/A")}")
            peerDatabase.addPeer(address, Some(handshake.nodeNonce), Some(handshake.nodeName))
          } else {
            log.debug(s"Got handshake on inbound connection from $address with declared address ${declaredAddressOption.getOrElse("N/A")}")
            if (declaredAddressOption.isDefined)
              peerDatabase.addPeer(declaredAddressOption.get, Some(handshake.nodeNonce), Some(handshake.nodeName))
          }

          connectedPeers += address -> connection.copy(handshake = Some(handshake))
        }
    }

  private def blacklistOperations: Receive = {
    case RegisterBlacklistListener(listener) =>
      blacklistListeners += listener
      listener ! ExistedBlacklist(peerDatabase.getBlacklist.toSeq)

    case UnregisterBlacklistListener(listener) =>
      blacklistListeners -= listener

    case BlacklistResendRequired =>
      blacklistListeners.foreach { listener =>
        listener ! ExistedBlacklist(peerDatabase.getBlacklist.toSeq)
      }

    case AddToBlacklist(address) =>
      addPeerToBlacklist(address)

    case Suspect(address) =>
      val count = suspects.getOrElse(address, 0)
      suspects.put(address, count + 1)
      if (count >= settings.blackListThreshold) {
        suspects.remove(address)
        addPeerToBlacklist(address)
      }
  }

  private def addPeerToBlacklist(address: InetSocketAddress): Unit = {
    log.info(s"Blacklist peer $address")
    peerDatabase.blacklistHost(address.getHostName)
    connectedPeers.remove(address).foreach(_.handlerRef ! CloseConnection)

    blacklistListeners.foreach { listener =>
      listener ! BlackListUpdated(address.getHostName)
    }
  }

  private def handshakedPeers = connectedPeers.filter(_._2.handshake.isDefined).keySet

}

object PeerManager {

  case class AddPeer(address: InetSocketAddress)

  case class Connected(socketAddress: InetSocketAddress, handlerRef: ActorRef,
                       ownSocketAddress: Option[InetSocketAddress], inbound: Boolean)

  case class Handshaked(address: InetSocketAddress, handshake: Handshake)

  case class Disconnected(remote: InetSocketAddress)

  case class AddToBlacklist(address: InetSocketAddress)

  case class GetRandomPeersToBroadcast(howMany: Int)

  case class ConnectedPeers(peers: Set[ConnectedPeer])

  case class PeerConnection(handlerRef: ActorRef, handshake: Option[Handshake], inbound: Boolean)

  case class BlackListUpdated(host: String)

  case class RegisterBlacklistListener(listener: ActorRef)

  case class UnregisterBlacklistListener(listener: ActorRef)

  case class ExistedBlacklist(hosts: Seq[String])

  case class Suspect(address: InetSocketAddress)

  case object CheckPeers

  case object GetAllPeers

  case object GetBlacklistedPeers

  case object GetConnectedPeers

  case object GetConnectedPeersTyped

  case object GetConnections

  private case object MarkConnectedPeersVisited

  case object BlacklistResendRequired

  case object CloseAllConnections

  case object CloseAllConnectionsComplete

}
