package scorex.transaction

import play.api.libs.json.Json
import scorex.account.PublicKeyAccount
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.crypto.hash.FastCryptographicHash

trait SignedTransaction extends TypedTransaction {
  def toSign: Array[Byte]

  val signature: Array[Byte]
  val sender: PublicKeyAccount
  protected lazy val signatureValid = EllipticCurveImpl.verify(signature, toSign, sender.publicKey)
  override lazy val id: Array[Byte] = FastCryptographicHash(toSign)

  protected def jsonBase() = Json.obj("type" -> transactionType.id,
    "id" -> Base58.encode(id),
    "sender" -> sender.address,
    "senderPublicKey" -> Base58.encode(sender.publicKey),
    "fee" -> assetFee._2,
    "timestamp" -> timestamp,
    "signature" -> Base58.encode(this.signature)
  )


  def validate: ValidationResult.Value
}
