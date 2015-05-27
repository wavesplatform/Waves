package scorex.network.message

import com.google.common.primitives.Bytes

case class GetBlockMessage(signature: Array[Byte]) extends Message {
  import SignaturesSeqMessage.SIGNATURE_LENGTH

  require(signature.length == SIGNATURE_LENGTH, "Data does not match length")

  override val messageType = Message.GET_BLOCK_TYPE

  override def toBytes() = Bytes.concat(super.toBytes(), generateChecksum(signature), signature)

  override def getDataLength() = SIGNATURE_LENGTH
}