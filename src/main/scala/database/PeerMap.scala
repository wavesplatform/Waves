package database

import java.net.InetAddress
import java.util
import java.util.ArrayList
import java.util.Arrays
import java.util.HashMap
import network.Peer
import org.mapdb.BTreeKeySerializer
import org.mapdb.DB
import com.google.common.primitives.UnsignedBytes
import scala.collection.JavaConversions._
import scala.util.Try



trait PeerMap extends DBMap[Array[Byte], Array[Byte]] {
	private val BYTE_WHITELISTED = Array[Byte](0, 0)
	private val BYTE_BLACKLISTED = Array[Byte](0, 0)
	
	private val observableData = new HashMap[Integer, Integer]()

	protected def createIndexes(database:DB){}

	override def getMap(database:DB)= database.createTreeMap("peers")
				.keySerializer(BTreeKeySerializer.BASIC)
				.comparator(UnsignedBytes.lexicographicalComparator())
				.makeOrGet()


	override def getMemoryMap() = new util.TreeMap[Array[Byte], Array[Byte]](UnsignedBytes.lexicographicalComparator())


	override protected def getDefaultValue() = null

	override def reset() = super.reset()

	override protected def getObservableData() = observableData

	
	def getKnownPeers(amount:Int) =
		Try{
			//GET ITERATOR
			val iterator = getKeys().iterator()
			
			//PEERS
			val peers = new ArrayList[Peer]()
			
			//ITERATE AS LONG AS:
			// 1. we have not reached the amount of peers
			// 2. we have read all records
			while(iterator.hasNext() && peers.size() < amount)
			{
				//GET ADDRESS
				val addressBI = iterator.next()
				
				//CHECK IF ADDRESS IS WHITELISTED
				if(Arrays.equals(this.get(addressBI), BYTE_WHITELISTED))
				{
					val address = InetAddress.getByAddress(addressBI)
					
					//CHECK IF SOCKET IS NOT LOCALHOST
					if(!address.equals(InetAddress.getLocalHost()))
					{
						//CREATE PEER
						val peer = new Peer(address)
						
						//ADD TO LIST
						peers.add(peer)
					}
				}			
			}
			
			//RETURN
			peers
		}.recover { case t:Throwable =>
			t.printStackTrace()
			
			new ArrayList[Peer]()
		}.getOrElse(new ArrayList[Peer]())

	
	def addPeer(peer:Peer)= map.put(peer.address.getAddress, BYTE_WHITELISTED)

	
	def blacklistPeer(peer:Peer){
		//TODO DISABLED WHILE UNSTABLE
	}
	
	def isBlacklisted(address:InetAddress) = {
		//CHECK IF PEER IS BLACKLISTED
		if(this.contains(address.getAddress)) get(address.getAddress).sameElements(BYTE_BLACKLISTED)
		else false
	}
}

object PeerMap {
	def apply(databaseSet:DBSet, database:DB) = new DBMap[Array[Byte], Array[Byte]](databaseSet, database) with PeerMap
	def apply(parent:PeerMap) = new DBMap(parent) with PeerMap
}

object PeerMapJava {
	def apply(databaseSet:DBSet, database:DB):PeerMap = new DBMap[Array[Byte], Array[Byte]](databaseSet, database) with PeerMap
	def apply(parent:PeerMap):PeerMap = new DBMap(parent) with PeerMap
}