package com.wavesplatform.account.protobuf
import com.google.common.primitives.Bytes
import com.wavesplatform.account.{Address, AddressOrAlias, Alias}
import com.wavesplatform.common.utils._

trait PBRecipientCompanionBase {
  implicit def apply(addressOrAlias: AddressOrAlias): Recipient = addressOrAlias match {
    case a: Address => a
    case al: Alias  => al
  }

  implicit def apply(address: Address): Recipient = {
    Recipient(address.bytes(1), Recipient.Recipient.Address(Recipient.Address(address.bytes.drop(2))))
  }

  implicit def apply(alias: Alias): Recipient = {
    Recipient(alias.chainId, Recipient.Recipient.Alias(Recipient.Alias(alias.name)))
  }

  implicit def toOption[A](address: A)(implicit ev: A => Recipient): Option[Recipient] = {
    Option(ev(address))
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
