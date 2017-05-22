package scorex.network

import java.net.InetSocketAddress

import scorex.network.message.Message

object NetworkController {
  case class DataFromPeer[V](messageType: Message.MessageCode, data: V, source: ConnectedPeer)

  case class SendToNetwork(message: Message[_ <: AnyRef], sendingStrategy: SendingStrategy)

  case class ConnectTo(address: InetSocketAddress)
}
