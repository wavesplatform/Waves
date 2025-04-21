package com.wavesplatform.protobuf.utils

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.{Amount, *}
import com.wavesplatform.protobuf.transaction.*
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

object PBImplicitConversions {
  import com.google.protobuf.{ByteString as PBByteString}
  import com.wavesplatform.{account as va}

  implicit class AddressOrAliasToPBExt(val r: va.AddressOrAlias) extends AnyVal {
    def toPB: Recipient = r match {
      case va.Alias(_, name) => Recipient.of(Recipient.Recipient.Alias(name))
      case w: va.Address     => Recipient.of(Recipient.Recipient.PublicKeyHash(PBByteString.copyFrom(w.publicKeyHash)))

    }
  }

  implicit class PBRecipientImplicitConversionOps(val recipient: Recipient) extends AnyVal {
    def toAddress(chainId: Byte): Either[ValidationError, va.Address]               = PBRecipients.toAddress(recipient, chainId)
    def toAlias(chainId: Byte): Either[ValidationError, va.Alias]                   = PBRecipients.toAlias(recipient, chainId)
    def toAddressOrAlias(chainId: Byte): Either[ValidationError, va.AddressOrAlias] = PBRecipients.toAddressOrAlias(recipient, chainId)
  }

  implicit def fromAssetIdAndAmount(v: (VanillaAssetId, Long)): Amount = v match {
    case (IssuedAsset(assetId), amount) =>
      Amount()
        .withAssetId(assetId.toByteString)
        .withAmount(amount)

    case (Waves, amount) =>
      Amount().withAmount(amount)
  }

  implicit class AmountImplicitConversions(val a: Amount) extends AnyVal {
    def longAmount: Long      = a.amount
    def vanillaAssetId: Asset = PBAmounts.toVanillaAssetId(a.assetId)
  }
}
