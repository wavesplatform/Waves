package network

import java.net.InetAddress

import network.message.Message

trait ConnectionCallback {
  def onConnect(peer: ConnectedPeer): Unit

  def onDisconnect(peer: ConnectedPeer): Unit

  def onError(peer: ConnectedPeer): Unit

  def isConnectedTo(address: InetAddress): Boolean

  def isConnectedTo(peer: Peer): Boolean

  def getActiveConnections: Seq[ConnectedPeer]

  def onMessage(message: Message): Unit
}
