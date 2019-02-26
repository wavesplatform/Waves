package com.wavesplatform.account.protobuf
import com.wavesplatform.account.{AddressOrAlias, Address => VAddress, Alias => VAlias}
import com.wavesplatform.common.utils._

trait PBRecipientCompanionBase {
  def empty: Recipient = Recipient.defaultInstance

  implicit def fromAddressOrAlias(addressOrAlias: AddressOrAlias): Recipient = addressOrAlias match {
    case a: VAddress  => a
    case al: VAddress => al
  }

  implicit def fromAddress(address: VAddress): Recipient = {
    Address(address)
  }

  implicit def fromAlias(alias: VAlias): Recipient = {
    Alias(alias.chainId, alias.name)
  }

  implicit class PBRecipientImplicitConversionOps(recipient: Recipient) {
    def toAddress: VAddress = recipient match {
      case Address(address) => address
      case _                => throw new IllegalArgumentException(s"Not an address: $recipient")
    }

    def toAlias: VAlias = recipient match {
      case Alias(chainId, name) =>
        VAlias
          .buildAlias(chainId.byte, name)
          .explicitGet()

      case _ =>
        throw new IllegalArgumentException(s"Not an alias: $recipient")
    }

    def toAddressOrAlias: AddressOrAlias = recipient match {
      case _: Alias        => this.toAlias
      case _: Address      => this.toAddress
      case Recipient.Empty => throw new IllegalArgumentException("Empty address not supported")
    }
  }
}
