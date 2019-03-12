package com.wavesplatform.protobuf.utils
import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.protobuf.account.{Alias, Recipient}
import com.wavesplatform.protobuf.transaction.{Amount, AssetAmount, VanillaAssetId}
import com.wavesplatform.transaction.ValidationError

private[protobuf] object PBInternalImplicits {
  import com.google.protobuf.{ByteString => PBByteString}
  import com.wavesplatform.account.{AddressOrAlias, Address => VAddress, Alias => VAlias}

  implicit def byteStringToByteStr(bs: PBByteString): ByteStr = bs.toByteArray
  implicit def byteStrToByteString(bs: ByteStr): PBByteString = PBByteString.copyFrom(bs)

  implicit def fromAddressOrAlias(addressOrAlias: AddressOrAlias): Recipient = addressOrAlias match {
    case a: VAddress => fromAddress(a)
    case al: VAlias  => fromAlias(al)
  }

  implicit def fromAddress(address: VAddress): Recipient = {
    Recipient.defaultInstance.withAddress(address.bytes)
  }

  implicit def fromAlias(alias: VAlias): Recipient = {
    Recipient.defaultInstance.withAlias(Alias(alias.chainId: Byte, alias.name))
  }

  implicit class PBRecipientImplicitConversionOps(recipient: Recipient) {
    def toAddress: Either[ValidationError, VAddress] = {
      VAddress.fromBytes(recipient.getAddress.toByteArray)
    }

    def toAlias: Either[ValidationError, VAlias] = {
      val alias = recipient.getAlias
      VAlias.buildAlias(if (alias.chainId.isEmpty) 0: Byte else alias.chainId.byteAt(0), alias.name)
    }

    def toAddressOrAlias: Either[ValidationError, AddressOrAlias] = recipient.recipient match {
      case Recipient.Recipient.Alias(_)   => this.toAlias
      case Recipient.Recipient.Address(_) => this.toAddress
      case Recipient.Recipient.Empty      => throw new IllegalArgumentException("Empty address not supported")
    }
  }

  implicit def fromAssetIdOptionAndAmount(v: (Option[VanillaAssetId], Long)): Amount = v match {
    case (Some(assetId), amount) =>
      Amount.defaultInstance.withAssetAmount(AssetAmount(assetId, amount))

    case (None, amount) =>
      Amount.defaultInstance.withWavesAmount(amount)
  }

  implicit def fromAssetIdAndAmount(v: (VanillaAssetId, Long)): Amount = {
    fromAssetIdOptionAndAmount((Option(v._1).filterNot(_.isEmpty), v._2))
  }

  implicit class AmountImplicitConversions(a: Amount) {
    def longAmount: Long = a.amount match {
      case Amount.Amount.Empty              => 0L
      case Amount.Amount.WavesAmount(value) => value
      case Amount.Amount.AssetAmount(value) => value.amount
    }

    def assetId: ByteStr = a.amount match {
      case Amount.Amount.WavesAmount(_) | Amount.Amount.Empty => ByteStr.empty
      case Amount.Amount.AssetAmount(AssetAmount(assetId, _)) => ByteStr(assetId.toByteArray)
    }
  }

  implicit class PBByteStringOps(bs: PBByteString) {
    def byteStr          = ByteStr(bs.toByteArray)
    def publicKeyAccount = PublicKeyAccount(bs.toByteArray)
  }

  implicit def byteStringToByte(bytes: ByteString): Byte =
    if (bytes.isEmpty) 0
    else bytes.byteAt(0)

  implicit def byteToByteString(chainId: Byte): ByteString = {
    if (chainId == 0) ByteString.EMPTY else ByteString.copyFrom(Array(chainId))
  }

  implicit def assetIdToAssetIdOption(assetId: VanillaAssetId): Option[VanillaAssetId] = Option(assetId).filterNot(_.isEmpty)
  implicit def assetIdOptionToAssetId(assetId: Option[VanillaAssetId]): VanillaAssetId = assetId.getOrElse(ByteStr.empty)
}
