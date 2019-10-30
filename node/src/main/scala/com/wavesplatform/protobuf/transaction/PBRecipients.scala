package com.wavesplatform.protobuf.transaction
import com.google.common.primitives.Bytes
import com.google.protobuf.ByteString
import com.wavesplatform.account._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError

object PBRecipients {
  def create(addressOrAlias: AddressOrAlias): Recipient = addressOrAlias match {
    case a: Address => Recipient().withAddress(ByteString.copyFrom(a.bytes.arr.slice(2, a.bytes.arr.length - Address.ChecksumLength)))
    case a: Alias   => Recipient().withAlias(a.name)
    case _          => sys.error("Should not happen " + addressOrAlias)
  }

  def toAddress(bytes: ByteStr): Either[ValidationError, Address] = bytes.length match {
    case Address.HashLength => // Compressed address
      val withHeader = Bytes.concat(Array(Address.AddressVersion, AddressScheme.current.chainId), bytes)
      val checksum   = Address.calcCheckSum(withHeader)
      Address.fromBytes(Bytes.concat(withHeader, checksum))

    case Address.AddressLength => // Regular address
      Address.fromBytes(bytes)

    case crypto.KeyLength => // Public key
      Right(PublicKey(bytes).toAddress)

    case _ =>
      Left(GenericError(s"Invalid address length: ${bytes.length}"))
  }

  def toAddress(r: Recipient): Either[ValidationError, Address] = r.recipient match {
    case Recipient.Recipient.Address(bytes) => toAddress(bytes.toByteArray)
    case _                                  => Left(GenericError(s"Not an address: $r"))
  }

  def toAlias(r: Recipient): Either[ValidationError, Alias] = r.recipient match {
    case Recipient.Recipient.Alias(alias) => Alias.create(alias)
    case _                                => Left(GenericError(s"Not an alias: $r"))
  }

  def toAddressOrAlias(r: Recipient): Either[ValidationError, AddressOrAlias] = {
    if (r.recipient.isAddress) toAddress(r)
    else if (r.recipient.isAlias) toAlias(r)
    else Left(GenericError(s"Not an address or alias: $r"))
  }
}
