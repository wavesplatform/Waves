package com.wavesplatform.transaction.protobuf

import com.google.protobuf.ByteString
import com.wavesplatform.account.{AddressScheme, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.serialization.protobuf.{PBMappers, PBSerializable, PBSerializableUnsigned}
import com.wavesplatform.{transaction => vt}

trait PBTransactionImplicits { self: PBMappers with PBSignedTransactionImplicits with PBAmountImplicits =>
  import InternalImplicits._

  implicit val PBTransactionPBSerializableInstance = new PBSerializable[PBTransaction] with PBSerializableUnsigned[PBTransaction] {
    override def protoBytes(value: PBTransaction): SerializedT         = PBTransactionSerialization.signedBytes(value.asSigned)
    override def protoBytesUnsigned(value: PBTransaction): SerializedT = PBTransactionSerialization.unsignedBytes(value)
  }

  implicit class VanillaOrderImplicitConversionOps(order: vt.assets.exchange.Order) {
    def toPB: ExchangeTransactionData.Order = {
      ExchangeTransactionData.Order(
        order.senderPublicKey,
        order.matcherPublicKey,
        Some(ExchangeTransactionData.Order.AssetPair(order.assetPair.amountAsset.get, order.assetPair.priceAsset.get)),
        order.orderType match {
          case vt.assets.exchange.OrderType.BUY  => ExchangeTransactionData.Order.Side.BUY
          case vt.assets.exchange.OrderType.SELL => ExchangeTransactionData.Order.Side.SELL
        },
        order.amount,
        order.price,
        order.timestamp,
        order.expiration,
        Some(order.matcherFee),
        order.version,
        order.proofs.map(bs => bs: ByteString)
      )
    }
  }

  implicit class PBOrderImplicitConversionOps(order: ExchangeTransactionData.Order) {
    def toVanillaWithVersion(version: Int): vt.assets.exchange.Order = {
      vt.assets.exchange.Order(
        order.senderPublicKey,
        order.matcherPublicKey,
        vt.assets.exchange.AssetPair(Some(order.getAssetPair.amountAssetId.toByteArray), Some(order.getAssetPair.priceAssetId.toByteArray)),
        order.orderSide match {
          case ExchangeTransactionData.Order.Side.BUY             => vt.assets.exchange.OrderType.BUY
          case ExchangeTransactionData.Order.Side.SELL            => vt.assets.exchange.OrderType.SELL
          case ExchangeTransactionData.Order.Side.Unrecognized(v) => throw new IllegalArgumentException(s"Unknown order type: $v")
        },
        order.amount,
        order.price,
        order.timestamp,
        order.expiration,
        order.getMatcherFee,
        order.proofs.map(_.toByteArray: ByteStr),
        version,
        order.matcherFee.map(_.assetId)
      )
    }

    def toVanilla: vt.assets.exchange.Order = toVanillaWithVersion(order.version.toInt)
  }

  implicit class PBTransactionImplicitConversionOps(tx: PBTransaction) {
    def asSigned: PBSignedTransaction = PBSignedTransaction(Some(tx))

    def withCurrentChainId: Transaction = tx.withChainId(AddressScheme.current.chainId: ChainId)

    def isLegacy: Boolean = tx.version == 1 || tx.version == 2

    def toVanillaOrAdapter: VanillaTransaction = if (this.isLegacy) toVanilla else toVanillaAdapter

    def toVanillaAdapter = asSigned.toVanillaAdapter

    def toVanilla: VanillaTransaction = this.asSigned.toVanilla
  }

  private[this] object InternalImplicits {

    implicit def implicitByteStringToPublicKeyAccount(bs: ByteString): PublicKeyAccount = {
      PublicKeyAccount(bs.toByteArray)
    }

    implicit def implicitPublicKeyAccountToByteString(pk: PublicKeyAccount): ByteString = {
      ByteString.copyFrom(pk.publicKey)
    }

    implicit def implicitIntToByte(int: Int): Byte = {
      require(int >= 0 && int <= 0xFF, s"Byte overflow: $int")
      int.toByte
    }

    implicit def implicitAssetIdToOption(assetId: PBAssetId): Option[VanillaAssetId] =
      Option(assetId)
        .map(_.bytes)
        .filterNot(_.isEmpty)

    implicit def implicitAssetIdOptionToAssetId(assetId: Option[VanillaAssetId]): PBAssetId =
      assetId.fold(PBAssetId.Waves)(PBAssetId.fromBytes)
  }
}
