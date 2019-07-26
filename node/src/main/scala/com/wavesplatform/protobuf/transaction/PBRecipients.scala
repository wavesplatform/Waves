package com.wavesplatform.protobuf.transaction
import com.google.common.primitives.Bytes
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressOrAlias, AddressScheme, Alias}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError

object PBRecipients {
  val CompressedAddressLength = Address.AddressLength - 6

  def create(addressOrAlias: AddressOrAlias): Recipient = addressOrAlias match {
    case a: Address => Recipient().withAddress(ByteString.copyFrom(a.bytes.arr.slice(2, a.bytes.arr.length - Address.ChecksumLength)))
    case a: Alias   => Recipient().withAlias(a.name)
    case _          => sys.error("Should not happen " + addressOrAlias)
  }

  //noinspection ScalaDeprecation
  def toAddress(r: Recipient): Either[ValidationError, Address] = r.recipient match {
    case Recipient.Recipient.Address(bytes) =>
      if (bytes.size() == Address.AddressLength) {
        val withHeader = Bytes.concat(Array(Address.AddressVersion, AddressScheme.current.chainId), bytes.toByteArray)
        Address.fromBytes(Bytes.concat(withHeader, Address.calcCheckSum(withHeader)))
      } else if (bytes.size() == CompressedAddressLength) Address.fromBytes(bytes.toByteArray)
      else Left(GenericError(s"Invalid address length: ${bytes.size()}"))

    case _ =>
      Left(GenericError(s"Not an address: $r"))
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
