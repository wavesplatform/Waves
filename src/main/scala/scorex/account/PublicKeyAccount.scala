package scorex.account

import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.InvalidAddress
import scorex.transaction.{TransactionParser, ValidationError}
import scala.language.implicitConversions

class PublicKeyAccount(val publicKey: Array[Byte]) {

  override def equals(b: Any): Boolean = b match {
    case a: PublicKeyAccount => publicKey.sameElements(a.publicKey)
    case _ => false
  }

  override def hashCode(): Int = publicKey.hashCode()

}

object PublicKeyAccount {

  implicit def toAddress(publicKeyAccount: PublicKeyAccount): Account = Account(Account.addressFromPublicKey(publicKeyAccount.publicKey))

  def fromBase58String(s: String): Either[ValidationError, PublicKeyAccount] =
    if (s.length > TransactionParser.KeyStringLength) Left(InvalidAddress)
    else Base58.decode(s).toOption.map(new PublicKeyAccount(_)).toRight(InvalidAddress)
}
