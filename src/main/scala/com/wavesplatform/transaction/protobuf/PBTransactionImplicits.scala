package com.wavesplatform.transaction.protobuf

import com.wavesplatform.account.AddressScheme
import com.wavesplatform.{transaction => vt}

trait PBTransactionImplicits {
  implicit class VanillaOrderImplicitConversionOps(order: vt.assets.exchange.Order) {
    def toPB: ExchangeTransactionData.Order = {
      ExchangeTransactionData.Order(
        order.senderPublicKey,
        order.matcherPublicKey,
        Some(ExchangeTransactionData.Order.AssetPair(order.assetPair.amountAsset, order.assetPair.priceAsset)),
        order.orderType match {
          case vt.assets.exchange.OrderType.BUY  => ExchangeTransactionData.Order.Type.BUY
          case vt.assets.exchange.OrderType.SELL => ExchangeTransactionData.Order.Type.SELL
        },
        order.amount,
        order.price,
        order.timestamp,
        order.expiration,
        order.matcherFee,
        order.proofs,
        order.version
      )
    }
  }

  implicit class PBOrderImplicitConversionOps(order: ExchangeTransactionData.Order) {
    def toVanillaWithVersion(version: Int): vt.assets.exchange.Order = {
      vt.assets.exchange.Order(
        order.senderPublicKey,
        order.matcherPublicKey,
        vt.assets.exchange.AssetPair(order.getAssetPair.amountAssetId, order.getAssetPair.priceAssetId),
        order.orderType match {
          case ExchangeTransactionData.Order.Type.BUY             => vt.assets.exchange.OrderType.BUY
          case ExchangeTransactionData.Order.Type.SELL            => vt.assets.exchange.OrderType.SELL
          case ExchangeTransactionData.Order.Type.Unrecognized(v) => throw new IllegalArgumentException(s"Unknown order type: $v")
        },
        order.amount,
        order.price,
        order.timestamp,
        order.expiration,
        order.matcherFee,
        vt.Proofs(order.proofs),
        version
      )
    }

    def toVanilla: vt.assets.exchange.Order = toVanillaWithVersion(order.version.toInt)
  }

  implicit class PBTransactionImplicitConversionOps(tx: PBTransaction) {
    def asSigned: PBSignedTransaction = PBSignedTransaction(tx)

    def withCurrentChainId: Transaction = tx.withChainId(AddressScheme.current.chainId)

    def isLegacy: Boolean = tx.version == 1 || tx.version == 2

    def toVanillaOrAdapter: VanillaTransaction = if (this.isLegacy) toVanilla else toVanillaAdapter

    def toVanillaAdapter = tx.asSigned.toVanillaAdapter

    def toVanilla: VanillaTransaction = PBSignedTransactionImplicits.PBTransactionImplicitConversionOps(this.asSigned).toVanilla
  }

  private[this] implicit def implicitIntToByte(int: Int): Byte = {
    require(int >= 0 && int <= 0xFF, s"Byte overflow: $int")
    int.toByte
  }

  private[this] implicit def implicitAssetIdToOption(assetId: PBAssetId): Option[VanillaAssetId] =
    Option(assetId)
      .map(_.bytes)
      .filterNot(_.isEmpty)

  private[this] implicit def implicitAssetIdOptionToAssetId(assetId: Option[VanillaAssetId]): PBAssetId =
    assetId.fold(PBAssetId.Waves)(PBAssetId.fromBytes)
}
