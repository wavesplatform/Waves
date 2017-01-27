package scorex.transaction

import play.api.libs.json.{JsObject, Json}
import scorex.account.PublicKeyAccount
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash

trait SignedTransaction extends TypedTransaction {
  def toSign: Array[Byte]

  val signature: Array[Byte]
  val sender: PublicKeyAccount
  override lazy val id: Array[Byte] = FastCryptographicHash(toSign)

  protected def jsonBase(): JsObject = Json.obj("type" -> transactionType.id,
    "id" -> Base58.encode(id),
    "sender" -> sender.address,
    "senderPublicKey" -> Base58.encode(sender.publicKey),
    "fee" -> assetFee._2,
    "timestamp" -> timestamp,
    "signature" -> Base58.encode(this.signature)
  )
}

object SignedTransaction {
  def verify[A <: SignedTransaction](t: A): Either[ValidationError, A] =
    {

      if (EllipticCurveImpl.verify(t.signature, t.toSign, t.sender.publicKey)) {
        Right(t)
      } else {
        Left(ValidationError.InvalidSignature)
      }
    }
}
