package scorex.network.message

import com.google.common.primitives.Bytes
import scorex.network.ConnectedPeer

case class GetBlockMessage(signature: Array[Byte], mbSender: Option[ConnectedPeer] = None, mbId: Option[Int] = None) extends Message {

  override val messageType = Message.GET_BLOCK_TYPE

  override def toBytes() = Bytes.concat(super.toBytes(), generateChecksum(signature), signature)

  override def getDataLength() = GetBlockMessage.GET_BLOCK_LENGTH
}

object GetBlockMessage {
  private val GET_BLOCK_LENGTH = 128

  def apply(data: Array[Byte]): GetBlockMessage = {
    //CHECK IF DATA MATCHES LENGTH
    require(data.length == GET_BLOCK_LENGTH, "Data does not match length")

    new GetBlockMessage(data)
  }
}