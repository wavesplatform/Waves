package scorex.network

import java.net.InetSocketAddress

import scorex.network.message.Message

object NetworkController {
  case class ConnectTo(address: InetSocketAddress)
}
