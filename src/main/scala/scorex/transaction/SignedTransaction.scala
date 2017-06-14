package scorex.transaction

import com.wavesplatform.state2.ByteStr
import play.api.libs.json.{JsObject, Json}
import scorex.account.PublicKeyAccount
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash

trait SignedTransaction extends Transaction with Signed {
  def toSign: Array[Byte]

  val signature: ByteStr
  val sender: PublicKeyAccount
  override lazy val id: ByteStr = ByteStr(FastCryptographicHash(toSign))

  protected def jsonBase(): JsObject = Json.obj("type" -> transactionType.id,
    "id" -> id.base58,
    "sender" -> sender.address,
    "senderPublicKey" -> Base58.encode(sender.publicKey),
    "fee" -> assetFee._2,
    "timestamp" -> timestamp,
    "signature" -> this.signature.base58
  )

  lazy val signatureValid : Boolean = EllipticCurveImpl.verify(signature.arr, toSign, sender.publicKey)
}
