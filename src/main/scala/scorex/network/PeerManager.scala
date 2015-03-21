package scorex.network

import java.net.InetAddress
import java.util.logging.Logger
import scorex.database.PeerDatabaseImpl
import settings.Settings

import scala.collection.JavaConversions._

object PeerManager {

  private val DATABASE_PEERS_AMOUNT = 1000

  def getKnownPeers: List[Peer] = {
    val knownPeers = PeerDatabaseImpl.knownPeers()
    Logger.getGlobal.info("Peers retrieved from database : " + knownPeers.size)
    if (knownPeers.size < DATABASE_PEERS_AMOUNT) {
      val settingsPeers = Settings.knownPeers
      settingsPeers.addAll(knownPeers)
      Logger.getGlobal.info("Peers retrieved after settings : " + settingsPeers.size)
      settingsPeers.toList
    } else knownPeers.toList
  }

  def addPeer(peer: Peer) {
    if (!Settings.knownPeers.exists(_.address == peer.address)) {
      PeerDatabaseImpl.addPeer(peer)
    }
  }

  def blacklistPeer(peer: Peer) = PeerDatabaseImpl.blacklistPeer(peer)

  def isBlacklisted(address: InetAddress) = PeerDatabaseImpl.isBlacklisted(address)

  def isBlacklisted(peer: Peer) = PeerDatabaseImpl.isBlacklisted(peer.address)
}
