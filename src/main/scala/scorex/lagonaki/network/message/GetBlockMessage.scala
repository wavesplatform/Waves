package scorex.lagonaki.network.message

import scorex.crypto.EllipticCurveImpl

case class GetBlockMessage(signature: Array[Byte]) extends Message {

  require(signature.length == EllipticCurveImpl.SignatureLength, "Data does not match length")

  override val messageType = Message.GetBlockType

  override lazy val dataBytes = signature
}