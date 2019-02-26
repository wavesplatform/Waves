package com.wavesplatform.account.protobuf
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.common.utils._

trait PBRecipientCompanionBase {
  def empty: Recipient = Recipient.defaultInstance

  implicit def fromAddressOrAlias(addressOrAlias: AddressOrAlias): Recipient = addressOrAlias match {
    case a: Address => a
    case al: Alias  => al
  }

  implicit def fromAddress(address: Address): Recipient = {
    val chainId = address.ensuring(_.bytes.length > 2, "Too short address bytes").bytes(1)
    Recipient(chainId, Recipient.Recipient.Address(Recipient.Address(address.bytes.drop(2))))
  }

  implicit def fromAlias(alias: Alias): Recipient = {
    Recipient(alias.chainId, Recipient.Recipient.Alias(Recipient.Alias(alias.name)))
  }

  implicit class PBRecipientImplicitConversionOps(recipient: Recipient) {
    def toAddress: Address = {
      Address
        .fromBytes(Bytes.concat(Array(Address.AddressVersion, recipient.chainId.toByte), recipient.getAddress.address.arr), recipient.chainId.toByte)
        .explicitGet()
    }

    def toAlias: Alias = {
      Alias
        .buildAlias(recipient.chainId.toByte, recipient.getAlias.name)
        .explicitGet()
    }

    def toAddressOrAlias: AddressOrAlias = recipient.recipient match {
      case _: Recipient.Recipient.Alias   => this.toAlias
      case _: Recipient.Recipient.Address => this.toAddress
      case Recipient.Recipient.Empty      => throw new IllegalArgumentException("Empty address not supported")
    }
  }
}
