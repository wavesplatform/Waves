package scorex.network.peer

import java.net.InetSocketAddress

case class PeerInfo(timestamp: Long, nonce: Long, nodeName: String = "")

trait PeerDatabase {

  def addPeer(socketAddress: InetSocketAddress, nonce: Option[Long], name: Option[String]) : Unit

  def removePeer(socketAddress: InetSocketAddress): Unit

  def touch(socketAddress: InetSocketAddress): Unit

  def blacklistHost(host: String): Unit

  def getKnownPeers: Map[InetSocketAddress, PeerInfo]

  def getBlacklist: Set[String]

  def getRandomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress]

}

