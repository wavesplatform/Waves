package scorex.network.message

import java.net.{InetAddress, InetSocketAddress}
import java.util

import com.google.common.primitives.{Bytes, Ints}


case class PeersMessage(peers: Seq[InetSocketAddress]) extends Message {

  import scorex.network.message.PeersMessage._

  override val messageType = Message.PeersType

  override lazy val dataBytes = {
    val length = peers.size
    val lengthBytes = Bytes.ensureCapacity(Ints.toByteArray(length), DataLength, 0)

    peers.foldLeft(lengthBytes) { case (bs, peer) =>
      Bytes.concat(bs,
        peer.getAddress.getAddress, Bytes.ensureCapacity(Ints.toByteArray(peer.getPort), 4, 0))
    }
  }
}


object PeersMessage {
  private val AddressLength = 4
  private val PortLength = 4
  private val DataLength = 4

  def apply(data: Array[Byte]): PeersMessage = {
    //READ LENGTH
    val lengthBytes = util.Arrays.copyOfRange(data, 0, DataLength)
    val length = Ints.fromByteArray(lengthBytes)

    //CHECK IF DATA MATCHES LENGTH
    if (data.length != DataLength + (length * (AddressLength + PortLength)))
      throw new Exception("Data does not match length")

    val peers = (0 to length - 1).map { i =>
      val position = lengthBytes.length + (i * (AddressLength + PortLength))
      val addressBytes = util.Arrays.copyOfRange(data, position, position + AddressLength)
      val address = InetAddress.getByAddress(addressBytes)
      val portBytes = util.Arrays.copyOfRange(data, position + AddressLength, position + AddressLength + PortLength)
      new InetSocketAddress(address, Ints.fromByteArray(portBytes))
    }

    new PeersMessage(peers)
  }
}