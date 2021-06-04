package com.wavesplatform.account

import java.nio.ByteBuffer

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.TxValidationError._

trait AddressOrAlias {
  def stringRepr: String
  def bytes: Array[Byte]
  def chainId: Byte

  override def toString: String = stringRepr

  override def equals(obj: Any): Boolean = obj match {
    case a: AddressOrAlias => java.util.Arrays.equals(bytes, a.bytes)
    case _                 => false
  }

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes)
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
        buf.position(buf.position() - 1)
        val addressBytes = new Array[Byte](Address.AddressLength)
        buf.get(addressBytes)
        Address.fromBytes(addressBytes)

      case Alias.AddressVersion =>
        val chainId    = buf.get
        val aliasBytes = Deser.parseArrayWithLength(buf)
        Alias.createWithChainId(new String(aliasBytes, "UTF-8"), chainId)

      case _ =>
        Left(InvalidAddress("Unknown address/alias version"))
    }
  }

  def fromString(s: String): Either[ValidationError, AddressOrAlias] = {
    if (s.startsWith(Alias.Prefix))
      Alias.fromString(s)
    else Address.fromString(s)
  }

  def fromRide(r: Recipient): Either[ValidationError, AddressOrAlias] =
    r match {
      case Recipient.Address(bytes) => Address.fromBytes(bytes.arr)
      case Recipient.Alias(name)    => Alias.create(name)
    }
}
