package scorex.account

import scorex.transaction.ValidationError


trait AccountOrAlias {
  def stringRepr: String

  def bytes: Array[Byte]
}

object AccountOrAlias {
  def fromBytes(bytes: Array[Byte]): Either[ValidationError, AccountOrAlias] =
    if (bytes.head == 0)
      Right(Alias(""))
    else
      Account.fromBytes(bytes)

  def fromString(s: String): Either[ValidationError, AccountOrAlias] = {
    Account.fromBase58String(s) match {
      case Right(a) => Right(a)
      case Left(_) => Right(Alias(""))
    }
  }
}