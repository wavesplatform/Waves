package network

import java.net.InetAddress
import java.net.ServerSocket
import java.util.Collections
import java.util.Observable
import java.util.Observer
import java.util.TreeSet
import java.util.logging.Logger

import utils.ObserverMessage
import controller.Controller
import network.message._

import scala.util.Try

object Network extends Observable with ConnectionCallback {

	val PORT = 9084
	
	private val MAX_HANDLED_MESSAGES_SIZE = 10000

	private val connectedPeers = scala.collection.mutable.Buffer[ConnectedPeer]()
	
	private val handledMessages = Collections.synchronizedSortedSet(new TreeSet[String]())
	
	private var isRunning = true
	
	require (Network.isPortAvailable(Network.PORT), "Network port " + Network.PORT + " already in use!")
	
	start()
	
	
	private def start(){
		//START ConnectionCreator THREAD
		new ConnectionCreator(this).start()
		
		//START ConnectionAcceptor THREAD
		new ConnectionAcceptor(this).start()
	}

	override def onConnect(peer:ConnectedPeer) {
		Logger.getGlobal.info("Connection successfull : " + peer.address)
		
		//ADD TO CONNECTED PEERS
		connectedPeers.synchronized{
			connectedPeers += peer
		}
		
		//ADD TO WHITELIST
		PeerManager.addPeer(peer)
		
		//PASS TO CONTROLLER
		Controller.onConnect(peer)
		
		//NOTIFY OBSERVERS
		setChanged()
		notifyObservers(new ObserverMessage(ObserverMessage.ADD_PEER_TYPE, peer))		
		
		setChanged()
		notifyObservers(new ObserverMessage(ObserverMessage.LIST_PEER_TYPE, connectedPeers))		
	}

	override def onDisconnect(peer:ConnectedPeer) {

		Logger.getGlobal.info("Connection close : " + peer.address)
		
		//REMOVE FROM CONNECTED PEERS
		connectedPeers.synchronized{
			connectedPeers -= peer
		}
		
		//PASS TO CONTROLLER
		Controller.onDisconnect(peer)
		
		//CLOSE CONNECTION IF STILL ACTIVE
		peer.close()
		
		//NOTIFY OBSERVERS
		setChanged()
		notifyObservers(new ObserverMessage(ObserverMessage.REMOVE_PEER_TYPE, peer))		
		
		setChanged()
		notifyObservers(new ObserverMessage(ObserverMessage.LIST_PEER_TYPE, connectedPeers))		
	}
	
	override def onError(peer:ConnectedPeer) {
		
		Logger.getGlobal.warning("Connection error : " + peer.address)
		
		//REMOVE FROM CONNECTED PEERS
		connectedPeers.synchronized{
			connectedPeers -= peer
		}
		
		//ADD TO BLACKLIST
		PeerManager.blacklistPeer(peer)
		
		//PASS TO CONTROLLER
		Controller.onError(peer)
		
		//CLOSE CONNECTION IF STILL ACTIVE
		peer.close()
					
		//NOTIFY OBSERVERS
		setChanged()
		notifyObservers(new ObserverMessage(ObserverMessage.REMOVE_PEER_TYPE, peer))		
		
		setChanged()
		notifyObservers(new ObserverMessage(ObserverMessage.LIST_PEER_TYPE, connectedPeers))		
	}
	
	override def isConnectedTo(address:InetAddress) = connectedPeers.exists(_.address.equals(address))

	override def isConnectedTo(peer:Peer) = isConnectedTo(peer.address)

	
	override def getActiveConnections() = connectedPeers
	
	private def addHandledMessage(hash:Array[Byte]){
		try {
			handledMessages.synchronized {
				//CHECK IF LIST IS FULL
				if (handledMessages.size() > MAX_HANDLED_MESSAGES_SIZE)
					handledMessages.remove(handledMessages.first())

				handledMessages.add(new String(hash))
			}
		}catch{
			case t:Throwable => t.printStackTrace()
		}
	}

	override def onMessage(message:Message) {

		if(!isRunning) return

		message match {
			case _:TransactionMessage | _:BlockMessage =>
				handledMessages.synchronized{
					//CHECK IF NOT HANDLED ALREADY
					if(handledMessages.contains(new String(message.getHash))) return

					//ADD TO HANDLED MESSAGES
					addHandledMessage(message.getHash())
				}
			case _ =>
		}
		Controller.onMessage(message)
	}

	def broadcast(message:Message, exclude:List[Peer]){
		Logger.getGlobal.info("Broadcasting")

		Try {
			connectedPeers.foreach { peer =>
				if (peer != null && !exclude.contains(peer)) peer.sendMessage(message)
			}
		}.recover{case t:Throwable => t.printStackTrace()}
		
		Logger.getGlobal().info("Broadcasting end")
	}
	
	override def addObserver(o:Observer) {
		super.addObserver(o)
		
		//SEND CONNECTEDPEERS ON REGISTER
		o.update(this, new ObserverMessage(ObserverMessage.LIST_PEER_TYPE, connectedPeers))
	}
	
	def isPortAvailable(port:Int) = Try(new ServerSocket(port).close()).isSuccess

	def stop(){
		isRunning = false
		onMessage(null)
	}
}