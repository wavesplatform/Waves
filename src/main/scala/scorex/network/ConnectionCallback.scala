package scorex.network

import java.net.InetAddress
import scorex.network.message.Message

trait ConnectionCallback {
  def onConnect(peer: ConnectedPeer): Unit

  def onDisconnect(peer: ConnectedPeer): Unit

  def onError(peer: ConnectedPeer): Unit

  def isConnectedTo(address: InetAddress): Boolean

  def isConnectedTo(peer: Peer): Boolean

  def activeConnections(): Seq[ConnectedPeer]

  def onMessage(message: Message): Unit
}
