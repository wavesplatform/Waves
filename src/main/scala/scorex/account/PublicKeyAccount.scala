package scorex.account

import com.wavesplatform.utils.base58Length
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519
import scorex.transaction.validation.ValidationError.InvalidAddress

trait PublicKeyAccount {
  def publicKey: Array[Byte]

  override def equals(b: Any): Boolean = b match {
    case a: PublicKeyAccount => publicKey.sameElements(a.publicKey)
    case _                   => false
  }

  override def hashCode(): Int = publicKey.hashCode()

  override lazy val toString: String = this.toAddress.address
}

object PublicKeyAccount {

  val KeyStringLength: Int = base58Length(Curve25519.KeyLength)

  private case class PublicKeyAccountImpl(publicKey: Array[Byte]) extends PublicKeyAccount

  def apply(publicKey: Array[Byte]): PublicKeyAccount = PublicKeyAccountImpl(publicKey)

  implicit def toAddress(publicKeyAccount: PublicKeyAccount): Address = Address.fromPublicKey(publicKeyAccount.publicKey)

  implicit class PublicKeyAccountExt(pk: PublicKeyAccount) {
    def toAddress: Address = PublicKeyAccount.toAddress(pk)
  }

  def fromBase58String(s: String): Either[InvalidAddress, PublicKeyAccount] =
    (for {
      _     <- Either.cond(s.length <= KeyStringLength, (), "Bad public key string length")
      bytes <- Base58.decode(s).toEither.left.map(ex => s"Unable to decode base58: ${ex.getMessage}")
    } yield PublicKeyAccount(bytes)).left.map(err => InvalidAddress(s"Invalid sender: $err"))
}
