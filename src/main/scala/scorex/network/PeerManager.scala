package scorex.network

import java.net.InetSocketAddress
import java.util.logging.Logger
import scorex.database.PeerDatabaseImpl
import settings.Settings
import scala.collection.JavaConversions._
import scala.util.Random

object PeerManager {

  private val DATABASE_PEERS_AMOUNT = 1000

  def knownPeers(): Seq[InetSocketAddress] = {
    val knownPeers = PeerDatabaseImpl.knownPeers()
    Logger.getGlobal.info("Peers retrieved from database : " + knownPeers)
    if (knownPeers.size < DATABASE_PEERS_AMOUNT) {
      val allPeers = Settings.knownPeers ++ knownPeers
      Logger.getGlobal.info("Peers retrieved after settings : " + allPeers)
      allPeers
    } else knownPeers
  }

  def peerConnected(peer:InetSocketAddress):Unit = {
    addPeer(peer)
    PeerDatabaseImpl.addConnectedPeer(peer)
  }

  def peerDisconnected(peer:InetSocketAddress):Unit =
    PeerDatabaseImpl.removeConnectedPeer(peer)


  def randomPeer(): InetSocketAddress = {
    val peers = knownPeers()
    peers(Random.nextInt(peers.size))
  }

  def addPeer(peer: InetSocketAddress):Unit = {
    //require(peer.getPort == Settings.Port)
    if (!Settings.knownPeers.contains(peer)) PeerDatabaseImpl.addKnownPeer(peer)
  }

  def blacklistPeer(peer: InetSocketAddress) = {
    PeerDatabaseImpl.removeConnectedPeer(peer)
    PeerDatabaseImpl.blacklistPeer(peer)
  }

  def isBlacklisted(address: InetSocketAddress) = PeerDatabaseImpl.isBlacklisted(address)
}
