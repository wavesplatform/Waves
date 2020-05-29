package com.wavesplatform.protobuf.utils
import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.Amount
import com.wavesplatform.protobuf.transaction._
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}

object PBImplicitConversions {
  import com.google.protobuf.{ByteString => PBByteString}
  import com.wavesplatform.account.{AddressOrAlias, Address => VAddress, Alias => VAlias}

  implicit def fromAddressOrAlias(addressOrAlias: AddressOrAlias): Recipient = PBRecipients.create(addressOrAlias)
  implicit def fromAddress(address: VAddress): PBByteString                  = PBByteString.copyFrom(address.bytes)

  implicit class PBRecipientImplicitConversionOps(recipient: Recipient) {
    def toAddress(chainId: Byte): Either[ValidationError, VAddress]              = PBRecipients.toAddress(recipient, chainId)
    def toAlias(chainId: Byte): Either[ValidationError, VAlias]                  = PBRecipients.toAlias(recipient, chainId)
    def toAddressOrAlias(chainId: Byte): Either[ValidationError, AddressOrAlias] = PBRecipients.toAddressOrAlias(recipient, chainId)
  }

  implicit class ByteStrExt(val bs: ByteStr) extends AnyVal {
    def toByteString: PBByteString = ByteString.copyFrom(bs.arr)
  }

  implicit class ByteStringExt(val bs: ByteString) extends AnyVal {
    def toByteStr: ByteStr = ByteStr(bs.toByteArray)
  }

  implicit def fromAssetIdAndAmount(v: (VanillaAssetId, Long)): Amount = v match {
    case (IssuedAsset(assetId), amount) =>
      Amount()
        .withAssetId(assetId.toByteString)
        .withAmount(amount)

    case (Waves, amount) =>
      Amount().withAmount(amount)
  }

  implicit class AmountImplicitConversions(a: Amount) {
    def longAmount: Long      = a.amount
    def vanillaAssetId: Asset = PBAmounts.toVanillaAssetId(a.assetId)
  }

  implicit class PBByteStringOps(bs: PBByteString) {
    def byteStr: ByteStr            = ByteStr(bs.toByteArray)
    def publicKeyAccount: PublicKey = PublicKey(bs.toByteArray)
  }

  implicit def byteStringToByte(bytes: ByteString): Byte =
    if (bytes.isEmpty) 0
    else bytes.byteAt(0)

  implicit def byteToByteString(chainId: Byte): ByteString = {
    if (chainId == 0) ByteString.EMPTY else ByteString.copyFrom(Array(chainId))
  }
}
