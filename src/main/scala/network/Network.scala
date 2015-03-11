package network

import java.net.{InetAddress, ServerSocket}
import java.util
import java.util.logging.Logger
import java.util.{Collections, TreeSet}

import controller.Controller
import network.message._

import scala.collection.JavaConversions._
import scala.util.Try

object Network extends ConnectionCallback {

  val PORT = 9084

  private val MAX_HANDLED_MESSAGES_SIZE = 10000

  private val connectedPeers = Collections.synchronizedList(new util.ArrayList[ConnectedPeer]())

  private val handledMessages = Collections.synchronizedSortedSet(new TreeSet[String]())

  private var isRunning = true

  require(Network.isPortAvailable(Network.PORT), "Network port " + Network.PORT + " already in use!")

  start()

  override def onConnect(peer: ConnectedPeer) {
    Logger.getGlobal.info("Connection successfull : " + peer.address)

    //ADD TO CONNECTED PEERS
    connectedPeers += peer

    //ADD TO WHITELIST
    PeerManager.addPeer(peer)

    //PASS TO CONTROLLER
    Controller.onConnect(peer)
  }

  override def onDisconnect(peer: ConnectedPeer) {

    Logger.getGlobal.info("Connection close : " + peer.address)

    //REMOVE FROM CONNECTED PEERS
    connectedPeers -= peer

    //PASS TO CONTROLLER
    Controller.onDisconnect(peer)

    //CLOSE CONNECTION IF STILL ACTIVE
    peer.close()
  }

  override def onError(peer: ConnectedPeer) {

    Logger.getGlobal.warning("Connection error : " + peer.address)

    //REMOVE FROM CONNECTED PEERS
    connectedPeers -= peer

    //ADD TO BLACKLIST
    PeerManager.blacklistPeer(peer)

    //PASS TO CONTROLLER
    Controller.onError(peer)

    //CLOSE CONNECTION IF STILL ACTIVE
    peer.close()
  }

  override def isConnectedTo(peer: Peer) = isConnectedTo(peer.address)

  override def isConnectedTo(address: InetAddress) = connectedPeers.exists(_.address.equals(address))

  override def activeConnections() = connectedPeers

  def broadcast(message: Message, exclude: List[Peer]) {
    Logger.getGlobal.info("Broadcasting")

    Try {
      connectedPeers.foreach { peer =>
        if (peer != null && !exclude.contains(peer)) peer.sendMessage(message)
      }
    }.recover { case t: Throwable => t.printStackTrace()}

    Logger.getGlobal.info("Broadcasting end")
  }

  def isPortAvailable(port: Int) = Try(new ServerSocket(port).close()).isSuccess

  def stop() {
    isRunning = false
    onMessage(null)
  }

  override def onMessage(message: Message) {

    if (!isRunning) return

    message match {
      case _: TransactionMessage | _: BlockMessage =>
        handledMessages.synchronized {
          //CHECK IF NOT HANDLED ALREADY
          if (handledMessages.contains(new String(message.hash()))) return

          //ADD TO HANDLED MESSAGES
          addHandledMessage(message.hash())
        }
      case _ =>
    }
    Controller.onMessage(message)
  }

  private def addHandledMessage(hash: Array[Byte]) {
    try {
      handledMessages.synchronized {
        //CHECK IF LIST IS FULL
        if (handledMessages.size() > MAX_HANDLED_MESSAGES_SIZE)
          handledMessages.remove(handledMessages.first())

        handledMessages.add(new String(hash))
      }
    } catch {
      case t: Throwable => t.printStackTrace()
    }
  }

  private def start() {
    //START ConnectionCreator THREAD
    new ConnectionCreator(this).start()

    //START ConnectionAcceptor THREAD
    new ConnectionAcceptor(this).start()
  }
}