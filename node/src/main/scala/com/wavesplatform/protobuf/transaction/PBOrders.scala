package com.wavesplatform.protobuf.transaction

import com.wavesplatform.transaction as vt
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.order.AssetPair
import com.wavesplatform.protobuf.order.Order.{PriceMode, Sender}
import com.wavesplatform.protobuf.order.Order.PriceMode.{ASSET_DECIMALS, FIXED_DECIMALS, DEFAULT as DEFAULT_PRICE_MODE}
import com.wavesplatform.transaction.assets.exchange.OrderPriceMode.{AssetDecimals, FixedDecimals, Default as DefaultPriceMode}
import vt.assets.exchange.OrderAuthentication

object PBOrders {
  import com.wavesplatform.protobuf.utils.PBImplicitConversions.*

  def vanilla(order: PBOrder): VanillaOrder = {
    VanillaOrder(
      order.version.toByte,
      order.sender match {
        case Sender.SenderPublicKey(value) => OrderAuthentication.OrderProofs(PublicKey(value.toByteStr), order.proofs.map(_.toByteStr))
        case Sender.Eip712Signature(sig)   => OrderAuthentication.Eip712Signature(sig.toByteStr)
        case Sender.Empty                  => throw new IllegalArgumentException("Order should have either senderPublicKey or eip712Signature")
      },
      PublicKey(order.matcherPublicKey.toByteArray),
      vt.assets.exchange
        .AssetPair(PBAmounts.toVanillaAssetId(order.getAssetPair.amountAssetId), PBAmounts.toVanillaAssetId(order.getAssetPair.priceAssetId)),
      order.orderSide match {
        case PBOrder.Side.BUY             => vt.assets.exchange.OrderType.BUY
        case PBOrder.Side.SELL            => vt.assets.exchange.OrderType.SELL
        case PBOrder.Side.Unrecognized(v) => throw new IllegalArgumentException(s"Unknown order type: $v")
      },
      order.amount,
      order.price,
      order.timestamp,
      order.expiration,
      order.getMatcherFee.longAmount,
      PBAmounts.toVanillaAssetId(order.getMatcherFee.assetId),
      order.priceMode match {
        case DEFAULT_PRICE_MODE        => DefaultPriceMode
        case ASSET_DECIMALS            => AssetDecimals
        case FIXED_DECIMALS            => FixedDecimals
        case PriceMode.Unrecognized(v) => throw new IllegalArgumentException(s"Unknown order price mode: $v")
      }
    )
  }

  def protobuf(order: VanillaOrder): PBOrder = {
    PBOrder(
      AddressScheme.current.chainId,
      order.matcherPublicKey.toByteString,
      Some(AssetPair(PBAmounts.toPBAssetId(order.assetPair.amountAsset), PBAmounts.toPBAssetId(order.assetPair.priceAsset))),
      order.orderType match {
        case vt.assets.exchange.OrderType.BUY  => PBOrder.Side.BUY
        case vt.assets.exchange.OrderType.SELL => PBOrder.Side.SELL
      },
      order.amount,
      order.price,
      order.timestamp,
      order.expiration,
      Some((order.matcherFeeAssetId, order.matcherFee)),
      order.version,
      order.proofs.map(_.toByteString),
      order.priceMode match {
        case DefaultPriceMode => DEFAULT_PRICE_MODE
        case AssetDecimals    => ASSET_DECIMALS
        case FixedDecimals    => FIXED_DECIMALS
      },
      order.orderAuthentication match {
        case OrderAuthentication.OrderProofs(key, _)        => Sender.SenderPublicKey(key.toByteString)
        case OrderAuthentication.Eip712Signature(signature) => Sender.Eip712Signature(signature.toByteString)
      }
    )
  }
}
