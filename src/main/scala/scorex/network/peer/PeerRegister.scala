package scorex.network.peer

import java.net.{InetAddress, InetSocketAddress}

import akka.actor.ActorRef
import scorex.network.Handshake
import scorex.utils.ScorexLogging

import scala.collection.mutable

trait PeerState

case object UnknownPeer extends PeerState

case object ConnectingPeer extends PeerState

case object ConnectedPeer extends PeerState

case object HandshakedPeer extends PeerState

class PeerRegister extends ScorexLogging {
  private val outboundOngoingConnections = mutable.HashSet[InetSocketAddress]()

  private val connectionHandlers = mutable.Map[InetSocketAddress, (Boolean, ActorRef)]()

  private val handshakedConnections = mutable.Map[InetSocketAddress, (Boolean, ActorRef, Handshake)]()

  private val suspectedConnections = mutable.Map[InetSocketAddress, Int]()

  // Ongoing connections support
  def initiateOutboundConnection(address: InetSocketAddress): Unit = outboundOngoingConnections.add(address)

  // Established but not handshaked support
  def registerHandler(address: InetSocketAddress, handler: ActorRef): Boolean = {
    val inbound = !outboundOngoingConnections.remove(address)
    connectionHandlers.put(address, (inbound, handler))
    inbound
  }

  // Handshaked connections support
  def registerHandshake(address: InetSocketAddress, handshake: Handshake): Unit = {
    val maybeValue = connectionHandlers.remove(address)
    if (maybeValue.isDefined) {
      val (inbound, handler) = maybeValue.get
      handshakedConnections.put(address, (inbound, handler, handshake))
    }
  }

  def outboundHandshakedConnectionsCount: Int = handshakedConnections.count(!_._2._1)

  def inboundHandshakedConnectionsCount: Int = handshakedConnections.count(_._2._1)

  def handshakedAddresses: Set[InetSocketAddress] = handshakedConnections.keySet.toSet

  def handshakedPeers: List[(InetSocketAddress, Handshake)] = handshakedConnections.map(kv => (kv._1, kv._2._3)).toList

  // Handlers retrieval
  def hasConnectionHandlers: Boolean = connectionHandlers.nonEmpty || handshakedConnections.nonEmpty

  def connectedPeerHandlers: Seq[ActorRef] =
    connectionHandlers.values.map(_._2).toSeq ++ handshakedConnections.values.map(_._2).toSeq

  // Address
  def isRegistered(address: InetSocketAddress): Boolean = outboundOngoingConnections.contains(address) ||
    connectionHandlers.contains(address) || handshakedConnections.contains(address)

  def getStageOfAddress(address: InetSocketAddress): PeerState =
    if (outboundOngoingConnections.contains(address)) ConnectingPeer
    else if (connectionHandlers.contains(address)) ConnectedPeer
    else if (handshakedConnections.contains(address)) HandshakedPeer
    else UnknownPeer


  // Host
  def getConnectionHandlersByHost(host: InetAddress): Seq[ActorRef] =
    connectionHandlers.filterKeys(_.getAddress == host).values.map(_._2).toSeq ++
      handshakedConnections.filterKeys(_.getAddress == host).values.map(_._2).toSeq

  def hostConnectionsCount(host: InetAddress): Int = getConnectionHandlersByHost(host).size

  def isNonceRegisteredForHost(host: InetAddress, nonce: Long): Boolean =
    handshakedConnections.filterKeys(_.getAddress == host).count(kv => kv._2._3.nodeNonce == nonce) != 0

  // Nonce
  def handshakedHandlersWithNonce: scala.Seq[(Long, ActorRef)] =
    handshakedConnections.values.map(v => (v._3.nodeNonce, v._2)).toSeq

  def getNonceOfHandshakedAddress(address: InetSocketAddress): Long = handshakedConnections(address)._3.nodeNonce

  def remove(address: InetSocketAddress): Unit = {
    outboundOngoingConnections.remove(address)
    connectionHandlers.remove(address)
    handshakedConnections.remove(address)
  }

  def isInbound(address: InetSocketAddress): Boolean = handshakedConnections(address)._1

  def getHandshakedHandler(address: InetSocketAddress): ActorRef = handshakedConnections(address)._2

  // Suspecting support
  def suspect(address: InetSocketAddress): Int = {
    val count = suspectedConnections.getOrElse(address, 0) + 1
    suspectedConnections.put(address, count)
    count
  }

  def removeSuspect(address: InetSocketAddress): Unit = suspectedConnections.remove(address)

  // Logging support
  def logConnections: String = {
    val inboundHandshakedAddresses = handshakedConnections.filter(_._2._1).keys
    val outboundHandshakedAddresses = handshakedConnections.filter(!_._2._1).keys
    val ongoingAddresses = outboundOngoingConnections
    val inboundConnectedAddresses = connectionHandlers.filter(_._2._1).keys
    val outboundConnectedAddresses = connectionHandlers.filter(!_._2._1).keys

    val ongoingLog = if (log.logger.isTraceEnabled) s"[${ongoingAddresses.mkString(";")}]" else ""
    val inboundHandshakedLog = if (log.logger.isTraceEnabled) s"[${inboundHandshakedAddresses.mkString(";")}]" else ""
    val outboundHandshakedLog = if (log.logger.isTraceEnabled) s"[${outboundHandshakedAddresses.mkString(";")}]" else ""
    val inboundConnectedLog = if (log.logger.isTraceEnabled) s"[${inboundConnectedAddresses.mkString(";")}]" else ""
    val outboundConnectedLog = if (log.logger.isTraceEnabled) s"[${outboundConnectedAddresses.mkString(";")}]" else ""

    s"Connections: Ongoing: (${ongoingAddresses.size})$ongoingLog: " +
      s"Established: (i:${inboundConnectedAddresses.size}|o:${outboundConnectedAddresses.size})$inboundConnectedLog$outboundConnectedLog: " +
      s"Handshaked: (i:${inboundHandshakedAddresses.size}|o:${outboundHandshakedAddresses.size})$inboundHandshakedLog$outboundHandshakedLog"
  }

}
