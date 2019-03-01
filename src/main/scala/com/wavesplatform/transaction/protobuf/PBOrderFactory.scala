package com.wavesplatform.transaction.protobuf
import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.{transaction => vt}

object PBOrderFactory {
  import PBInternalImplicits._

  def create(order: ExchangeTransactionData.Order, version: Int = 0) = {
    vt.assets.exchange.Order(
      PublicKeyAccount(order.senderPublicKey.toByteArray),
      PublicKeyAccount(order.matcherPublicKey.toByteArray),
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
      order.getMatcherFee.longAmount,
      order.proofs.map(_.toByteArray: ByteStr),
      if (version == 0) order.version.toByte else version.toByte,
      order.matcherFee.map(_.assetId)
    )
  }

  def protobuf(order: vt.assets.exchange.Order) = {
    ExchangeTransactionData.Order(
      ByteString.copyFrom(order.senderPublicKey.publicKey),
      ByteString.copyFrom(order.matcherPublicKey.publicKey),
      Some(ExchangeTransactionData.Order.AssetPair(order.assetPair.amountAsset.get, order.assetPair.priceAsset.get)),
      order.orderType match {
        case vt.assets.exchange.OrderType.BUY  => ExchangeTransactionData.Order.Side.BUY
        case vt.assets.exchange.OrderType.SELL => ExchangeTransactionData.Order.Side.SELL
      },
      order.amount,
      order.price,
      order.timestamp,
      order.expiration,
      Some((order.matcherFeeAssetId, order.matcherFee)),
      order.version,
      order.proofs.map(bs => bs: ByteString)
    )
  }
}
