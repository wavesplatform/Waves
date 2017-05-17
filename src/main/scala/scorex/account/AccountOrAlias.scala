package scorex.account

import scorex.serialization.Deser
import scorex.transaction.ValidationError


trait AccountOrAlias {
  def stringRepr: String

  def bytes: Array[Byte]

  override def toString = stringRepr
}

object AccountOrAlias {

  def fromBytes(bytes: Array[Byte], position: Int): Either[ValidationError, (AccountOrAlias, Int)] = {
    bytes(position) match {
      case Account.AddressVersion =>
        val addressEnd = position + Account.AddressLength
        val addressBytes = bytes.slice(position, addressEnd)
        Account.fromBytes(addressBytes).map((_, addressEnd))
      case Alias.AddressVersion =>
        val (_, aliasEnd) = Deser.parseArraySize(bytes, position + 2)
        Alias.fromBytes(bytes.slice(position, aliasEnd)).map((_, aliasEnd))
      case _ => Left(ValidationError.InvalidAddress)
    }
  }

  def fromString(s: String): Either[ValidationError, AccountOrAlias] = {
    if (s.startsWith(Alias.Prefix))
      Alias.fromString(s)
    else Account.fromString(s)
  }
}