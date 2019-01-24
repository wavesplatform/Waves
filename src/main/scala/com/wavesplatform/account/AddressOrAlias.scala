package com.wavesplatform.account
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.ValidationError

trait AddressOrAlias {
  def stringRepr: String

  def bytes: ByteStr

  override def toString: String = stringRepr

  override def equals(obj: scala.Any): Boolean = obj match {
    case a: AddressOrAlias => bytes == a.bytes
    case _                 => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes.arr)
}

object AddressOrAlias {

  def fromBytes(bytes: Array[Byte], position: Int): Either[ValidationError, (AddressOrAlias, Int)] = {
    bytes(position) match {
      case Address.AddressVersion =>
        val addressEnd   = position + Address.AddressLength
        val addressBytes = bytes.slice(position, addressEnd)
        Address.fromBytes(addressBytes).map((_, addressEnd))
      case Alias.AddressVersion =>
        val (_, aliasEnd) = Deser.parseArraySize(bytes, position + 2)
        Alias.fromBytes(bytes.slice(position, aliasEnd)).map((_, aliasEnd))
      case _ => Left(ValidationError.InvalidAddress("Unknown address/alias version"))
    }
  }

  def fromString(s: String): Either[ValidationError, AddressOrAlias] = {
    if (s.startsWith(Alias.Prefix))
      Alias.fromString(s)
    else Address.fromString(s)
  }
}
