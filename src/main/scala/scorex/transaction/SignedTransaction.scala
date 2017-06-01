package scorex.transaction

import com.wavesplatform.state2.{ByteArray, EqByteArray}
import play.api.libs.json.{JsObject, Json}
import scorex.account.PublicKeyAccount
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash

trait SignedTransaction extends Transaction {
  def toSign: Array[Byte]

  val signature: ByteArray
  val sender: PublicKeyAccount
  override lazy val id: ByteArray = EqByteArray(FastCryptographicHash(toSign))

  protected def jsonBase(): JsObject = Json.obj("type" -> transactionType.id,
    "id" -> id.base58,
    "sender" -> sender.address,
    "senderPublicKey" -> Base58.encode(sender.publicKey),
    "fee" -> assetFee._2,
    "timestamp" -> timestamp,
    "signature" -> this.signature.base58
  )
}

object SignedTransaction {
  def verify[A <: SignedTransaction](t: A): Either[ValidationError, A] =
    {
      if (EllipticCurveImpl.verify(t.signature.arr, t.toSign, t.sender.publicKey)) {
        Right(t)
      } else {
        Left(ValidationError.InvalidSignature)
      }
    }
}
