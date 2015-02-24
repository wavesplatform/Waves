package network.message

import com.google.common.primitives.{Bytes, Ints}
import network.ConnectedPeer

case class VersionMessage(height: Int, mbSender: Option[ConnectedPeer] = None, mbId: Option[Int] = None) extends Message {

  override val messageType = Message.VERSION_TYPE

  override def toBytes() = {
    val heightBytes = Bytes.ensureCapacity(Ints.toByteArray(height), VersionMessage.HEIGHT_LENGTH, 0)
    Bytes.concat(super.toBytes(), generateChecksum(heightBytes), heightBytes)
  }

  override def getDataLength() = VersionMessage.HEIGHT_LENGTH
}


object VersionMessage {
  private val HEIGHT_LENGTH = 4

  def apply(data: Array[Byte]): VersionMessage = {
    //CHECK IF DATA MATCHES LENGTH
    require(data.length == HEIGHT_LENGTH, "Data does not match length")
    val height = Ints.fromByteArray(data)
    new VersionMessage(height)
  }
}
