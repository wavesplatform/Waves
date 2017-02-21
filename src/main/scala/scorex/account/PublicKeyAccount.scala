package scorex.account

import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError.InvalidAddress
import scorex.transaction.{TypedTransaction, ValidationError}

@SerialVersionUID(-5511437096393374460L)
class PublicKeyAccount(val publicKey: Array[Byte]) extends Account(Account.addressFromPublicKey(publicKey))

object PublicKeyAccount {
  def fromBase58String(s: String): Either[ValidationError, PublicKeyAccount] =
    if (s.length > TypedTransaction.KeyStringLength) Left(InvalidAddress)
    else Base58.decode(s).toOption.map(new PublicKeyAccount(_)).toRight(InvalidAddress)
}
