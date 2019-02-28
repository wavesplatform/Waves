package com.wavesplatform.account.protobuf
import com.wavesplatform.account.{AddressOrAlias, Address => VAddress, Alias => VAlias}
import com.wavesplatform.common.utils._

trait PBRecipientCompanionBase {
  def empty: Recipient = Recipient.defaultInstance

  implicit def fromAddressOrAlias(addressOrAlias: AddressOrAlias): Recipient = addressOrAlias match {
    case a: VAddress => fromAddress(a)
    case al: VAlias  => fromAlias(al)
  }

  implicit def fromAddress(address: VAddress): Recipient = {
    Recipient()
      .withAddress(address)
  }

  implicit def fromAlias(alias: VAlias): Recipient = {
    Recipient()
      .withAlias(Alias(alias.chainId, alias.name))
  }

  implicit class PBRecipientImplicitConversionOps(recipient: Recipient) {
    def toAddress: VAddress = {
      recipient.getAddress
    }

    def toAlias: VAlias = {
      val alias = recipient.getAlias
      VAlias
        .buildAlias(alias.chainId.byte, alias.name)
        .explicitGet()
    }

    def toAddressOrAlias: AddressOrAlias = recipient.recipient match {
      case Recipient.Recipient.Alias(_)         => this.toAlias
      case Recipient.Recipient.Address(address) => address
      case Recipient.Recipient.Empty            => throw new IllegalArgumentException("Empty address not supported")
    }
  }
}
