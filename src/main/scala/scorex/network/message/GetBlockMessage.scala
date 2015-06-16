package scorex.network.message

case class GetBlockMessage(signature: Array[Byte]) extends Message {
  import SignaturesSeqMessage.SIGNATURE_LENGTH

  require(signature.length == SIGNATURE_LENGTH, "Data does not match length")

  override val messageType = Message.GET_BLOCK_TYPE

  override lazy val dataBytes = signature
}