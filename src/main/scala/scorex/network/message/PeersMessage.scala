package scorex.network.message

import java.net.{InetAddress, InetSocketAddress}
import java.util

import com.google.common.primitives.{Bytes, Ints}
import scorex.app.settings.Settings


case class PeersMessage(peers: Seq[InetSocketAddress]) extends Message {

  import scorex.network.message.PeersMessage._

  override val messageType = Message.PeersType

  override lazy val dataBytes = {
    val length = peers.size
    val lengthBytes = Bytes.ensureCapacity(Ints.toByteArray(length), DataLength, 0)

    peers.foldLeft(lengthBytes) { case (bytes, peer) =>
      Bytes.concat(bytes, peer.getAddress.getAddress)
    }
  }
}


object PeersMessage {
  private val AddressLength = 4
  private val DataLength = 4

  def apply(data: Array[Byte]): PeersMessage = {
    //READ LENGTH
    val lengthBytes = util.Arrays.copyOfRange(data, 0, DataLength)
    val length = Ints.fromByteArray(lengthBytes)

    //CHECK IF DATA MATCHES LENGTH
    if (data.length != DataLength + (length * AddressLength))
      throw new Exception("Data does not match length")

    val peers = (0 to length - 1).map { i =>
      val position = lengthBytes.length + (i * AddressLength)
      val addressBytes = util.Arrays.copyOfRange(data, position, position + AddressLength)
      val address = InetAddress.getByAddress(addressBytes)
      new InetSocketAddress(address, Settings.Port)
    }

    new PeersMessage(peers)
  }
}