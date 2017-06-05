package scorex.account

import com.wavesplatform.state2.ByteStr
import scorex.serialization.Deser
import scorex.transaction.ValidationError


trait AccountOrAlias {
  def stringRepr: String

  def bytes: ByteStr

  override def toString: String = stringRepr

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: AccountOrAlias => bytes == a.bytes
    case _ => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes.arr)
}

object AccountOrAlias {

  def fromBytes(bytes: Array[Byte], position: Int): Either[ValidationError, (AccountOrAlias, Int)] = {
    bytes(position) match {
      case Account.AddressVersion =>
        val addressEnd = position + Account.AddressLength
        val addressBytes = bytes.slice(position, addressEnd)
        Account.fromBytes(addressBytes).map((_, addressEnd))
      case Alias.AddressVersion =>
        val (arr, aliasEnd) = Deser.parseArraySize(bytes, position + 2)
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