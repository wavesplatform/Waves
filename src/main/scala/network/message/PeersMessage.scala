package network.message

import java.net.InetAddress
import java.util

import com.google.common.primitives.{Bytes, Ints}
import network.{ConnectedPeer, Peer}

case class PeersMessage(peers: Seq[Peer], mbSender: Option[ConnectedPeer] = None, mbId: Option[Int] = None) extends Message {

  import network.message.PeersMessage._

  override val messageType = Message.PEERS_TYPE

  override def toBytes() = {
    val length = peers.size
    val lengthBytes = Bytes.ensureCapacity(Ints.toByteArray(length), DATA_LENGTH, 0)

    //WRITE PEERS
    val data = peers.foldLeft(lengthBytes) { case (bytes, peer) =>
      Bytes.concat(bytes, peer.address.getAddress)
    }

    //ADD CHECKSUM
    Bytes.concat(super.toBytes(), generateChecksum(data), data)
  }

  override def getDataLength() = DATA_LENGTH + (peers.size * ADDRESS_LENGTH)
}


object PeersMessage {
  private val ADDRESS_LENGTH = 4
  private val DATA_LENGTH = 4

  def apply(data: Array[Byte]): PeersMessage = {
    //READ LENGTH
    val lengthBytes = util.Arrays.copyOfRange(data, 0, DATA_LENGTH)
    val length = Ints.fromByteArray(lengthBytes)

    //CHECK IF DATA MATCHES LENGTH
    if (data.length != DATA_LENGTH + (length * ADDRESS_LENGTH))
      throw new Exception("Data does not match length")

    val peers = (0 to length - 1).map { i =>
      val position = lengthBytes.length + (i * ADDRESS_LENGTH)
      val addressBytes = util.Arrays.copyOfRange(data, position, position + ADDRESS_LENGTH)
      val address = InetAddress.getByAddress(addressBytes)
      new Peer(address)
    }


    new PeersMessage(peers)
  }
}