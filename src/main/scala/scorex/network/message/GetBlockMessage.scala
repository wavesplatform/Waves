package scorex.network.message

case class GetBlockMessage(signature: Array[Byte]) extends Message {

  require(signature.length == scorex.crypto.SigningFunctionsImpl.SignatureLength, "Data does not match length")

  override val messageType = Message.GetBlockType

  override lazy val dataBytes = signature
}