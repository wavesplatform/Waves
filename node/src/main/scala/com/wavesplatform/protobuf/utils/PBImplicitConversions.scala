package com.wavesplatform.protobuf.utils

import com.wavesplatform.account.EthereumAddress
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.transaction._
import com.wavesplatform.protobuf.{Amount, _}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

object PBImplicitConversions {
  import com.google.protobuf.{ByteString => PBByteString}
  import com.wavesplatform.{account => va}

  implicit class RecipientExt(val r: va.Recipient) extends AnyVal {
    def toPb: Recipient = r match {
      case va.Alias(_, name)     => Recipient.of(Recipient.Recipient.Alias(name))
      case w: va.WavesAddress    => Recipient.of(Recipient.Recipient.PublicKeyHash(PBByteString.copyFrom(w.publicKeyHash)))
      case e: va.EthereumAddress => Recipient.of(Recipient.Recipient.EthereumAddress(PBByteString.copyFrom(e.publicKeyHash)))

    }
  }

  implicit class PBRecipientImplicitConversionOps(val recipient: Recipient) extends AnyVal {
    def toAddress(chainId: Byte): Either[ValidationError, va.Address]               = PBRecipients.toAddress(recipient, chainId)
    def toAlias(chainId: Byte): Either[ValidationError, va.Alias]                   = PBRecipients.toAlias(recipient, chainId)
    def toAddressOrAlias(chainId: Byte): Either[ValidationError, va.AddressOrAlias] = PBRecipients.toAddressOrAlias(recipient, chainId)
    def toRecipient(chainId: Byte): Either[ValidationError, va.Recipient]           = Right(new EthereumAddress(recipient.recipient.publicKeyHash.map(_.toByteArray)getOrElse(Array[Byte]())))    //TODO remove
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
