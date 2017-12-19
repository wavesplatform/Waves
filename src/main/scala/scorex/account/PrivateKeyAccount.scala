package scorex.account

import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.GenericError

sealed trait PrivateKeyAccount extends PublicKeyAccount {
  def seed: Array[Byte]

  def privateKey: Array[Byte]
}

object PrivateKeyAccount {

  private case class PrivateKeyAccountImpl(seed: Array[Byte], privateKey: Array[Byte], publicKey: Array[Byte]) extends PrivateKeyAccount

  def apply(seed: Array[Byte]): PrivateKeyAccount = {
    val pair = EllipticCurveImpl.createKeyPair(seed)
    PrivateKeyAccountImpl(seed, pair._1, pair._2)
  }

  def fromSeed(s: String): Either[GenericError, PrivateKeyAccount] = Base58.decode(s)
    .toEither
    .right.map(PrivateKeyAccount(_))
    .left.map(ex => GenericError(s"Unable to get a private key from the seed '$s': ${ex.getMessage}"))

}
