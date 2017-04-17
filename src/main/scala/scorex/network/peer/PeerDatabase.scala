package scorex.network.peer

import java.net.InetSocketAddress

@SerialVersionUID(-8490103514095092420L)
case class PeerInfo(timestamp: Long, nonce: Long, nodeName: String = "")

trait PeerDatabase {

  def addPeer(socketAddress: InetSocketAddress, nonce: Option[Long], name: Option[String])

  def removePeer(socketAddress: InetSocketAddress)

  def touch(socketAddress: InetSocketAddress)

  def blacklistHost(host: String)

  def getKnownPeers: Map[InetSocketAddress, PeerInfo]

  def getBlacklist: Set[String]

  def getRandomPeer(excluded: Set[InetSocketAddress]): Option[InetSocketAddress]

}

