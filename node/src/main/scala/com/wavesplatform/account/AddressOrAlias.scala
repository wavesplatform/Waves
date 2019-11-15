package com.wavesplatform.account
import java.nio.ByteBuffer

import com.google.common.primitives.Bytes
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.TxValidationError._

trait AddressOrAlias {
  def stringRepr: String
  def bytes: ByteStr

  override def toString: String = stringRepr

  override def equals(obj: Any): Boolean = obj match {
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
        val (_, aliasEnd) = Deser.parseArrayWithLength(bytes, position + 2)
        Alias.fromBytes(bytes.slice(position, aliasEnd)).map((_, aliasEnd))
      case _ => Left(InvalidAddress("Unknown address/alias version"))
    }
  }

  def fromBytes(buf: ByteBuffer): Either[ValidationError, AddressOrAlias] = {
    buf.get match {
      case Address.AddressVersion =>
        val addressBytes = new Array[Byte](Address.AddressLength - 1)
        buf.get(addressBytes)
        Address.fromBytes(Bytes.concat(Array(Address.AddressVersion), addressBytes))

      case Alias.AddressVersion =>
        val chainId = buf.get
        val aliasBytes = Deser.parseArrayWithLength(buf)
        Alias.fromBytes(Bytes.concat(Array(Address.AddressVersion, chainId), aliasBytes))

      case _ =>
        Left(InvalidAddress("Unknown address/alias version"))
    }
  }

  def fromString(s: String): Either[ValidationError, AddressOrAlias] = {
    if (s.startsWith(Alias.Prefix))
      Alias.fromString(s)
    else Address.fromString(s)
  }
}
