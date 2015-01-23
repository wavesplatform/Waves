package network

import java.net.InetAddress
import java.util.logging.Logger

import database.DBSet
import settings.Settings
import scala.collection.JavaConversions._

object PeerManager {

	private val DATABASE_PEERS_AMOUNT = 1000

	def getKnownPeers: List[Peer] = {
		val knownPeers = DBSet.getInstance().getPeerMap.getKnownPeers(DATABASE_PEERS_AMOUNT)
		Logger.getGlobal.info("Peers retrieved from database : " + knownPeers.size)
		if(knownPeers.size() < DATABASE_PEERS_AMOUNT) {
			val settingsPeers = Settings.getKnownPeers
			settingsPeers.addAll(knownPeers)
			Logger.getGlobal.info("Peers retrieved after settings : " + settingsPeers.size())
			settingsPeers.toList
		}else knownPeers.toList
	}
	
	def addPeer(peer:Peer){
		if(!Settings.getKnownPeers.exists(_.address == peer.address)) {
			DBSet.getInstance().getPeerMap.addPeer(peer)
		}
	}
	
	def blacklistPeer(peer:Peer) = DBSet.getInstance().getPeerMap.blacklistPeer(peer)

	def isBlacklisted(address:InetAddress) = DBSet.getInstance().getPeerMap.isBlacklisted(address)

	def isBlacklisted(peer:Peer) = DBSet.getInstance().getPeerMap.isBlacklisted(peer.address)
}
