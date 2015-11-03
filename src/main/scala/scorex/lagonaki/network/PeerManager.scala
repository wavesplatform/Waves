package scorex.lagonaki.network

import java.net.InetSocketAddress
import scorex.lagonaki.app.LagonakiSettings
import scorex.transaction.state.database.PeerDatabaseImpl
import scorex.utils.ScorexLogging

import scala.util.Random

class PeerManager(settings: LagonakiSettings) extends ScorexLogging {
  private val DatabasePeersAmount = 1000

  def knownPeers(): Seq[InetSocketAddress] = {
    val knownPeers = PeerDatabaseImpl.knownPeers()
    log.info("Peers retrieved from database : " + knownPeers)
    if (knownPeers.size < DatabasePeersAmount) {
      val allPeers = settings.knownPeers ++ knownPeers
      log.info("Peers retrieved including settings : " + allPeers)
      allPeers
    } else knownPeers
  }

  def peerConnected(peer: InetSocketAddress): Unit = {
    PeerDatabaseImpl.addConnectedPeer(peer)
  }

  def peerDisconnected(peer: InetSocketAddress): Unit =
    PeerDatabaseImpl.removeConnectedPeer(peer)

  def randomPeer(): Option[InetSocketAddress] = {
    val peers = knownPeers()
    if(peers.nonEmpty) Some(peers(Random.nextInt(peers.size)))
    else None
  }

  def addPeer(peer: InetSocketAddress): Unit =
    if (!settings.knownPeers.contains(peer))
      PeerDatabaseImpl.addKnownPeer(peer)


  def blacklistPeer(peer: InetSocketAddress) = {
    PeerDatabaseImpl.removeConnectedPeer(peer)
    PeerDatabaseImpl.blacklistPeer(peer)
  }

  def isBlacklisted(address: InetSocketAddress) = PeerDatabaseImpl.isBlacklisted(address)
}