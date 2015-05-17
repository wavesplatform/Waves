package scorex.network

import java.net.InetSocketAddress
import java.util.logging.Logger
import scorex.database.PeerDatabaseImpl
import settings.Settings
import ch.qos.logback.classic.LoggerContext
import org.slf4j.LoggerFactory

import scala.util.Random

object PeerManager {
  def logger = LoggerFactory.getLogger(this.getClass)

  private val DATABASE_PEERS_AMOUNT = 1000

  def knownPeers(): Seq[InetSocketAddress] = {
    val knownPeers = PeerDatabaseImpl.knownPeers()
    logger.info("Peers retrieved from database : " + knownPeers)
    if (knownPeers.size < DATABASE_PEERS_AMOUNT) {
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
