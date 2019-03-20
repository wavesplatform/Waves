package com.wavesplatform.protobuf.transaction
import com.google.common.primitives.Bytes
import com.google.protobuf.ByteString
import com.wavesplatform.account.{Address, AddressOrAlias, AddressScheme, Alias}
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.GenericError

object PBRecipients {
  def create(addressOrAlias: AddressOrAlias): Recipient = addressOrAlias match {
    case a: Address => Recipient().withAddress(ByteString.copyFrom(a.bytes.arr.slice(2, a.bytes.arr.length - Address.ChecksumLength)))
    case a: Alias   => Recipient().withAlias(a.name)
    case _          => sys.error("Should not happen " + addressOrAlias)
  }

  //noinspection ScalaDeprecation
  def toAddress(r: Recipient): Either[ValidationError, Address] = r.recipient match {
    case Recipient.Recipient.Address(bytes) =>
      val withHeader = Bytes.concat(Array(Address.AddressVersion, AddressScheme.current.chainId), bytes.toByteArray)
      Right(Address.createUnsafe(Bytes.concat(withHeader, Address.calcCheckSum(withHeader))))

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
