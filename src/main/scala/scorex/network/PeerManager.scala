package scorex.network

import java.net.InetSocketAddress

import org.slf4j.LoggerFactory
import scorex.database.PeerDatabaseImpl
import scorex.settings.Settings

import scala.util.Random

object PeerManager {
  lazy val logger = LoggerFactory.getLogger(this.getClass)

  private val DatabasePeersAmount = 1000

  def knownPeers(): Seq[InetSocketAddress] = {
    val knownPeers = PeerDatabaseImpl.knownPeers()
    logger.info("Peers retrieved from database : " + knownPeers)
    if (knownPeers.size < DatabasePeersAmount) {
      val allPeers = Settings.knownPeers ++ knownPeers
      logger.info("Peers retrieved including settings : " + allPeers)
      allPeers
    } else knownPeers
  }

  def peerConnected(peer: InetSocketAddress): Unit = {
    addPeer(peer)
    PeerDatabaseImpl.addConnectedPeer(peer)
  }

  def peerDisconnected(peer: InetSocketAddress): Unit =
    PeerDatabaseImpl.removeConnectedPeer(peer)


  def randomPeer(): InetSocketAddress = {
    val peers = knownPeers()
    peers(Random.nextInt(peers.size))
  }

  def addPeer(peer: InetSocketAddress): Unit = {
    //require(peer.getPort == Settings.Port)
    if (!Settings.knownPeers.contains(peer)) PeerDatabaseImpl.addKnownPeer(peer)
  }

  def blacklistPeer(peer: InetSocketAddress) = {
    PeerDatabaseImpl.removeConnectedPeer(peer)
    PeerDatabaseImpl.blacklistPeer(peer)
  }

  def isBlacklisted(address: InetSocketAddress) = PeerDatabaseImpl.isBlacklisted(address)
}
