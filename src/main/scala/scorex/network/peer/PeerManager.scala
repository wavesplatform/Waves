package scorex.network.peer

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, Props}
import akka.event.LoggingReceive
import com.wavesplatform.Version
import com.wavesplatform.settings.{Constants, NetworkSettings}
import scorex.app.ApplicationVersion
import scorex.network.NetworkController.SendToNetwork
import scorex.network._
import scorex.network.message.MessageHandler.RawNetworkData
import scorex.network.message.{Message, MessageSpec}
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Random, Try}

/** Must be singleton */
class PeerManager(
    settings: NetworkSettings,
    networkController: ActorRef,
    applicationName: String,
    appVersion: ApplicationVersion) extends Actor with ScorexLogging {

  import PeerConnectionHandler._
  import PeerManager._

  private val blacklistResendInterval = settings.blackListResidenceTime / 10
  private val visitPeersInterval = settings.peersDataResidenceTime / 10

  private val maybeFilename = Option(settings.file).filter(_.trim.nonEmpty)
  private val peerDatabase: PeerDatabase = new PeerDatabaseImpl(settings, maybeFilename)

  context.system.scheduler.schedule(visitPeersInterval, visitPeersInterval, self, MarkConnectedPeersVisited)
  private val blacklistListeners: scala.collection.mutable.Set[ActorRef] = scala.collection.mutable.Set.empty[ActorRef]
  context.system.scheduler.schedule(blacklistResendInterval, blacklistResendInterval, self, BlacklistResendRequired)

  private val register = new PeerRegister()

  private var maybeShutdownRequester: Option[ActorRef] = None

  private val DefaultPort = 6863

  getKnownPeersAddresses(settings.knownPeers).foreach(peerDatabase.addPeer(_, Some(0), None))

  private def getKnownPeersAddresses(knownPeers: List[String]): Seq[InetSocketAddress] = {
    Try {
      knownPeers.map { peer =>
        val addressParts = peer.split(":").map(_.trim)
        val port = if (addressParts.length == 2) addressParts(1).toInt else DefaultPort
        new InetSocketAddress(addressParts(0), port)
      }
    }.getOrElse(Seq[InetSocketAddress]())
  }

  override def receive: Receive = LoggingReceive(({
    case CheckPeers =>
      if (register.outboundHandshakedConnectionsCount < settings.maxOutboundConnections) {
        val maybeRandomPeerAddress = peerDatabase.getRandomPeer(register.handshakedAddresses.toSet)
        if (maybeRandomPeerAddress.isDefined) {
          val address = maybeRandomPeerAddress.get
          log.debug(s"Trying connect to random peer $address")
          register.initiateOutboundConnection(address)
          networkController ! NetworkController.ConnectTo(address)
        } else log.debug("Unable to get random peer")
      } else log.info(s"Outbound connections limit (${settings.maxOutboundConnections}) exceeded")

    case MarkConnectedPeersVisited => register.handshakedAddresses.foreach(peerDatabase.touch)

    case CloseAllConnections =>
      if (!register.hasConnectionHandlers) {
        sender() ! CloseAllConnectionsComplete
      } else {
        maybeShutdownRequester = Some(sender())
        register.connectedPeerHandlers.foreach(_ ! CloseConnection)
      }

    case CloseConnectionCompleted(remote) =>
      disconnect(remote)
      if (!register.hasConnectionHandlers) maybeShutdownRequester.foreach(_ ! CloseAllConnectionsComplete)

  }: Receive) orElse blacklistOperations orElse peerListOperations orElse peerCycle)

  private def peerCycle: Receive = {
    case Connected(remote, handlerRef, ownSocketAddress) =>
      log.debug(s"On new connection: ${register.logConnections}")

      if (isBlacklisted(remote)) {
        log.warn(s"New network connection with blacklisted peer '$remote': Close connection")
        handlerRef ! CloseConnection
      } else if (register.hostConnectionsCount(remote.getAddress) >= settings.maxConnectionsWithSingleHost) {
        log.warn(s"Number of network connections with host '${remote.getAddress}' exceed allowed ${settings.maxConnectionsWithSingleHost}: Close connection")
        handlerRef ! CloseConnection
      } else handleNewConnection(remote, handlerRef, ownSocketAddress)

    case Handshaked(address, handshake) =>
      if (isBlacklisted(address)) {
        log.warn(s"Got handshake from blacklisted $address")
        register.getConnectionHandlersByHost(address.getAddress).foreach(_ ! CloseConnection)
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

  private def peerListOperations: Receive = {
    case AddPeer(address) =>
      peerDatabase.addPeer(address, None, None)

    case GetConnectedPeers =>
      log.debug(s"Reporting connected peers: ${register.logConnections}")
      sender() ! register.handshakedPeers

    case GetConnectedPeersTyped =>
      val peers = register.handshakedPeers.map {
        case (address, handshake) => new InetAddressPeer(handshake.nodeNonce, address, self)
      }
      sender() ! ConnectedPeers(peers.toSet)

    case GetRandomPeersToBroadcast(howMany) =>
      val dbPeers = peerDatabase.getKnownPeers.keySet
      val intersection = dbPeers.intersect(register.handshakedAddresses.toSet)
      sender() ! Random.shuffle(intersection.toSeq).take(howMany)

    case GetAllPeers => sender() ! peerDatabase.getKnownPeers

    case GetBlacklistedPeers => sender() ! peerDatabase.getBlacklist
  }

  private def handleNewConnection(remote: InetSocketAddress,
                                  handlerRef: ActorRef,
                                  maybeDeclaredAddress: Option[InetSocketAddress]): Unit = {
    register.getStageOfAddress(remote) match {
      case UnknownPeer =>
        log.info(s"New inbound connection from '$remote'")
        if (register.inboundHandshakedConnectionsCount >= settings.maxInboundConnections) {
          log.warn(s"Number of inbound connections (${register.inboundHandshakedConnectionsCount}) " +
            s"exceed allowed ${settings.maxInboundConnections}: Close connection")
          handlerRef ! CloseConnection
        } else {
          register.registerHandler(remote, handlerRef)
          replyWithHandshake(handlerRef, maybeDeclaredAddress)
        }
      case ConnectingPeer =>
        log.info(s"New outbound connection to '$remote'")
        if (register.outboundHandshakedConnectionsCount >= settings.maxOutboundConnections) {
          log.warn(s"Number of outbound connections (${register.outboundHandshakedConnectionsCount}) " +
            s"exceed allowed ${settings.maxOutboundConnections}: Close connection")
          handlerRef ! CloseConnection
        } else {
          register.registerHandler(remote, handlerRef)
          replyWithHandshake(handlerRef, maybeDeclaredAddress)
        }
      case ConnectedPeer =>
        log.warn(s"Duplicate connection with already connected peer '$remote': Close connection")
        handlerRef ! CloseConnection
      case HandshakedPeer =>
        log.warn(s"Duplicate connection with already handshaked peer '$remote': Close connection")
        handlerRef ! CloseConnection
    }
  }

  private def replyWithHandshake(handler: ActorRef, maybeDeclaredAddress: Option[InetSocketAddress]): Unit = {
    val handshake = Handshake(applicationName, appVersion, settings.nodeName, settings.nonce, maybeDeclaredAddress,
      System.currentTimeMillis() / 1000)

    handler ! handshake
  }

  private def sendDataToNetwork(message: Message[_], sendingStrategy: SendingStrategy): Unit = {
    val peers = register.handshakedHandlersWithNonce
    val chosen = sendingStrategy.choose(peers)
    log.trace(s"${chosen.length} peers have been chosen among ${peers.size}")
    chosen.foreach(_._2 ! message)
  }

  private def processDataFromNetwork(spec: MessageSpec[_], msgData: Array[Byte], remote: InetSocketAddress): Unit = {
    register.getStageOfAddress(remote) match {
      case UnknownPeer =>
        log.error(s"Network message from unregistered peer '$remote'")
        sender() ! CloseConnection

      case ConnectingPeer =>
        log.error(s"Network message from not connected peer '$remote'")
        sender() ! CloseConnection

      case ConnectedPeer =>
        log.error(s"Network message from not handshaked peer '$remote'")
        sender() ! CloseConnection

      case HandshakedPeer =>
        val peer = new InetAddressPeer(register.getNonceOfHandshakedAddress(remote), remote, self)
        networkController ! Message(spec, Left(msgData), Some(peer))
    }
  }

  private def disconnect(remote: InetSocketAddress): Unit = {
    register.remove(remote)
    log.debug(s"After disconnect: ${register.logConnections}")
  }

  private def handleHandshake(remote: InetSocketAddress, handshake: Handshake): Unit =
    register.getStageOfAddress(remote) match {
      case UnknownPeer =>
        log.error(s"Handshake with unregistered peer '$remote': Close connection")
        sender() ! CloseConnection

      case HandshakedPeer =>
        log.error(s"Duplicate handshake from '$remote': Close connection")
        sender() ! CloseConnection

      case ConnectingPeer =>
        log.error(s"Handshake with not connected peer '$remote': Close connection")
        sender() ! CloseConnection

      case ConnectedPeer =>
        if (!isHandshakeAcceptable(handshake)) {
          log.warn(s"Unacceptable handshake (${handshake.applicationName}|${handshake.applicationVersion}) from '$remote': Move to blacklist")
          self ! AddToBlacklist(remote)
        } else if (settings.nonce == handshake.nodeNonce) {
          log.info("Drop connection to self")
          peerDatabase.removePeer(remote)
          register.getConnectedHandler(remote).foreach(_ ! CloseConnection)
        } else if (register.isNonceRegisteredForHost(remote.getAddress, handshake.nodeNonce)) {
          log.info(s"Duplicate connection from '$remote' with nonce '${handshake.nodeNonce}': Close connection")
          register.getConnectedHandler(remote).foreach(_ ! CloseConnection)
        } else {
          register.isConnectionInbound(remote).foreach { inbound =>
            val direction = if (inbound) "inbound" else "outbound"
            val maybeRemoteAddress = if (inbound) handshake.declaredAddress else Some(remote)
            log.debug(s"Got handshake on $direction with '$remote' with declared address '${handshake.declaredAddress.getOrElse("N/A")}'")
            maybeRemoteAddress.foreach(peerDatabase.addPeer(_, Some(handshake.nodeNonce), Some(handshake.nodeName)))
            register.registerHandshake(remote, handshake)
          }
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
      val count = register.suspect(address)
      log.debug(s"Peer '$address' is under suspicion with $count failures out of ${
        settings.blackListThreshold
      }")
      if (count >= settings.blackListThreshold) {
        register.removeSuspect(address)
        addPeerToBlacklist(address)
      }
  }

  private def addPeerToBlacklist(address: InetSocketAddress): Unit = {
    log.info(s"Host '${
      address.getHostName
    }' was blacklisted because of peer '$address'")
    peerDatabase.blacklistHost(address.getHostName)
    register.getConnectionHandlersByHost(address.getAddress).foreach(_ ! CloseConnection)

    blacklistListeners.foreach { listener =>
      listener ! BlackListUpdated(address.getHostName)
    }
  }

  private def isHandshakeAcceptable(handshake: Handshake): Boolean =
    applicationName == handshake.applicationName && handshake.applicationVersion.compatibleWith(appVersion)

}

object PeerManager {
  def props(networkSettings: NetworkSettings, networkController: ActorRef, addressSchemeCharacter: Char) =
    Props(new PeerManager(networkSettings, networkController, Constants.ApplicationName + addressSchemeCharacter, new ApplicationVersion(Version.VersionTuple)))

  case class AddPeer(address: InetSocketAddress)

  case class Connected(socketAddress: InetSocketAddress, handlerRef: ActorRef, maybeDeclaredAddress: Option[InetSocketAddress])

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

  private case object MarkConnectedPeersVisited

  case object BlacklistResendRequired

  case object CloseAllConnections

  case object CloseAllConnectionsComplete

}
