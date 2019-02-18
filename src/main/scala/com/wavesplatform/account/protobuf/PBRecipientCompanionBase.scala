package com.wavesplatform.account.protobuf
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.common.utils._

trait PBRecipientCompanionBase {
  def empty = Recipient.defaultInstance

  implicit def fromAddressOrAlias(addressOrAlias: AddressOrAlias): Recipient = addressOrAlias match {
    case a: Address => a
    case al: Alias  => al
  }

  implicit def fromAddress(address: Address): Recipient = {
    Recipient(address.bytes(1), Recipient.Recipient.Address(Recipient.Address(address.bytes.drop(2))))
  }

  implicit def fromAlias(alias: Alias): Recipient = {
    Recipient(alias.chainId, Recipient.Recipient.Alias(Recipient.Alias(alias.name)))
  }

  implicit class PBRecipientImplicitConversionOps(recipient: Recipient) {
    def toAddress =
      Address.fromBytes(Bytes.concat(Array(Address.AddressVersion, recipient.chainId.toByte), recipient.getAddress.address.arr)).explicitGet()
    def toAlias = Alias.buildAlias(recipient.chainId.toByte, recipient.getAlias.name).explicitGet()
    def toAddressOrAlias = recipient.recipient match {
      case Recipient.Recipient.Alias(alias)     => toAlias
      case Recipient.Recipient.Address(address) => toAddress
      case Recipient.Recipient.Empty            => throw new IllegalArgumentException("Empty address not supported")
    }
  }
}
