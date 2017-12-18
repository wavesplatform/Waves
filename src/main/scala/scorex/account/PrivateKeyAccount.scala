package scorex.account

import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser
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

  def fromBase58String(s: String): Either[GenericError, PrivateKeyAccount] =
    (for {
      _ <- Either.cond(s.length <= TransactionParser.KeyStringLength, (), "Bad private key string length")
      bytes <- Base58.decode(s).toEither.left.map(ex => s"Unable to decode base58: ${ex.getMessage}")
    } yield PrivateKeyAccount(bytes)).left.map(err => GenericError(s"Can't parse '$s' as private key: $err"))

}
