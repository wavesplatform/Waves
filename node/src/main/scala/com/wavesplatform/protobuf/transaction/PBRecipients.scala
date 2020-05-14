package com.wavesplatform.protobuf.transaction
import com.google.common.primitives.Bytes
import com.google.protobuf.ByteString
import com.wavesplatform.account._
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError

object PBRecipients {
  def create(addressOrAlias: AddressOrAlias): Recipient = addressOrAlias match {
    case a: Address => Recipient().withPublicKeyHash(ByteString.copyFrom(publicKeyHash(a)))
    case a: Alias   => Recipient().withAlias(a.name)
    case _          => sys.error("Should not happen " + addressOrAlias)
  }

  def toAddress(bytes: Array[Byte], chainId: Byte): Either[ValidationError, Address] = bytes.length match {
    case Address.HashLength => // Compressed address
      val withHeader = Bytes.concat(Array(Address.AddressVersion, chainId), bytes)
      val checksum   = Address.calcCheckSum(withHeader)
      Address.fromBytes(Bytes.concat(withHeader, checksum), chainId)

    case Address.AddressLength => // Regular address
      Address.fromBytes(bytes, chainId)

    case crypto.KeyLength => // Public key
      Right(PublicKey(bytes).toAddress(chainId))

    case _ =>
      Left(GenericError(s"Invalid address length: ${bytes.length}"))
  }

  def toAddress(r: Recipient, chainId: Byte): Either[ValidationError, Address] = r.recipient match {
    case Recipient.Recipient.PublicKeyHash(bytes) => toAddress(bytes.toByteArray, chainId)
    case _                                        => Left(GenericError(s"Not an address: $r"))
  }

  def toAlias(r: Recipient, chainId: Byte): Either[ValidationError, Alias] = r.recipient match {
    case Recipient.Recipient.Alias(alias) => Alias.createWithChainId(alias, chainId)
    case _                                => Left(GenericError(s"Not an alias: $r"))
  }

  def toAddressOrAlias(r: Recipient, chainId: Byte): Either[ValidationError, AddressOrAlias] = {
    if (r.recipient.isPublicKeyHash) toAddress(r, chainId)
    else if (r.recipient.isAlias) toAlias(r, chainId)
    else Left(GenericError(s"Not an address or alias: $r"))
  }

  @inline
  final def publicKeyHash(address: Address): Array[Byte] =
    address.bytes.slice(2, address.bytes.length - Address.ChecksumLength)
}
