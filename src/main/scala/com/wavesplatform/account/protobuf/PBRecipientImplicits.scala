package com.wavesplatform.account.protobuf
import com.wavesplatform.account.{AddressOrAlias, Address => VAddress, Alias => VAlias}
import com.wavesplatform.common.utils._
import com.wavesplatform.serialization.protobuf.utils._
import com.wavesplatform.transaction.protobuf.ChainId

trait PBRecipientImplicits {
  implicit def fromAddressOrAlias(addressOrAlias: AddressOrAlias): Recipient = addressOrAlias match {
    case a: VAddress => fromAddress(a)
    case al: VAlias  => fromAlias(al)
  }

  implicit def fromAddress(address: VAddress): Recipient = {
    Recipient.defaultInstance.withAddress(address.bytes)
  }

  implicit def fromAlias(alias: VAlias): Recipient = {
    Recipient.defaultInstance.withAlias(Alias(alias.chainId: ChainId, alias.name))
  }

  implicit class PBRecipientImplicitConversionOps(recipient: Recipient) {
    def toAddress: VAddress = {
      VAddress.fromBytes(recipient.getAddress.toByteArray).explicitGet()
    }

    def toAlias: VAlias = {
      val alias = recipient.getAlias
      VAlias
        .buildAlias(if (alias.chainId.isEmpty) 0: Byte else alias.chainId.byteAt(0), alias.name)
        .explicitGet()
    }

    def toAddressOrAlias: AddressOrAlias = recipient.recipient match {
      case Recipient.Recipient.Alias(_)   => this.toAlias
      case Recipient.Recipient.Address(_) => this.toAddress
      case Recipient.Recipient.Empty      => throw new IllegalArgumentException("Empty address not supported")
    }
  }
}
