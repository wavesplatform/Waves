package scorex.account

import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.InvalidAddress
import scorex.transaction.{TransactionParser, ValidationError}

import scala.language.implicitConversions


trait PublicKeyAccount {
  def publicKey: Array[Byte]

  override def equals(b: Any): Boolean = b match {
    case a: PublicKeyAccount => publicKey.sameElements(a.publicKey)
    case _ => false
  }

  override def hashCode(): Int = publicKey.hashCode()

  override lazy val toString: String = this.toAddress.address
}

object PublicKeyAccount {

  private case class PublicKeyAccountImpl(publicKey: Array[Byte]) extends PublicKeyAccount

  def apply(publicKey: Array[Byte]): PublicKeyAccount = PublicKeyAccountImpl(publicKey)

  implicit def toAddress(publicKeyAccount: PublicKeyAccount): Address = Address.fromPublicKey(publicKeyAccount.publicKey)

  implicit class PublicKeyAccountExt(pk: PublicKeyAccount) {
    def toAddress: Address = PublicKeyAccount.toAddress(pk)
  }

  def fromBase58String(s: String): Either[ValidationError, PublicKeyAccount] =
    if (s.length > TransactionParser.KeyStringLength) Left(InvalidAddress)
    else Base58.decode(s).toOption.map(PublicKeyAccount(_)).toRight(InvalidAddress)
}
