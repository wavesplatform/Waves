package network.message

import com.google.common.primitives.Bytes
import network.ConnectedPeer

case class GetSignaturesMessage(parent: Array[Byte], mbSender: Option[ConnectedPeer] = None, mbId: Option[Int] = None) extends Message {

  override val messageType = Message.GET_SIGNATURES_TYPE

  override def toBytes() = Bytes.concat(super.toBytes(), generateChecksum(parent), parent)

  override def getDataLength() = GetSignaturesMessage.GET_HEADERS_LENGTH
}


object GetSignaturesMessage {
  private val GET_HEADERS_LENGTH = 128

  def apply(data: Array[Byte]): GetSignaturesMessage = {
    //CHECK IF DATA MATCHES LENGTH
    require(data.length == GET_HEADERS_LENGTH, "Data does not match length")
    new GetSignaturesMessage(data)
  }
}
