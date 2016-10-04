package scorex.transaction

import scorex.account.PublicKeyAccount
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.hash.FastCryptographicHash

trait SignedTransaction extends TypedTransaction {
  def toSign: Array[Byte]

  val signature: Array[Byte]
  val sender: PublicKeyAccount
  lazy val signatureValid = EllipticCurveImpl.verify(signature, toSign, sender.publicKey)
  override lazy val id: Array[Byte] = FastCryptographicHash(toSign)

}
