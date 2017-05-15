package scorex.network.message

import java.net.InetSocketAddress

object MessageHandler {
  case class RawNetworkData(spec: MessageSpec[_ <: AnyRef], data: Array[Byte], remote: InetSocketAddress)
}
