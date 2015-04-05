package scorex.network.message

import com.google.common.primitives.Bytes


case class GetBlockMessage(signature: Array[Byte]) extends Message {
  private val GET_BLOCK_LENGTH = 128

  require(signature.length == GET_BLOCK_LENGTH, "Data does not match length")

  override val messageType = Message.GET_BLOCK_TYPE

  override def toBytes() = Bytes.concat(super.toBytes(), generateChecksum(signature), signature)

  override def getDataLength() = GET_BLOCK_LENGTH
}