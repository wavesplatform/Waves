package scorex.account

import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.InvalidAddress

@SerialVersionUID(-5511437096393374460L)
class PublicKeyAccount(val publicKey: Array[Byte]) extends Account(Account.addressFromPublicKey(publicKey))

object PublicKeyAccount {
  def fromBase58String(s: String): Either[ValidationError, PublicKeyAccount] =
    Base58.decode(s).toEither.left.map(_ => InvalidAddress).map(new PublicKeyAccount(_))
}
