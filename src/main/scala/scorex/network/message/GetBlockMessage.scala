package scorex.network.message

case class GetBlockMessage(signature: Array[Byte]) extends Message {
  import SignaturesSeqMessage.SignatureLength

  require(signature.length == SignatureLength, "Data does not match length")

  override val messageType = Message.GetBlockType

  override lazy val dataBytes = signature
}