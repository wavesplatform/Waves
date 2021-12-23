package com.wavesplatform.protobuf.transaction

import com.wavesplatform.transaction as vt
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.protobuf.*
import com.wavesplatform.protobuf.order.AssetPair
import com.wavesplatform.protobuf.order.Order.{PriceMode, Sender}
import com.wavesplatform.protobuf.order.Order.PriceMode.{ASSET_DECIMALS, DEFAULT, FIXED_DECIMALS}
import com.wavesplatform.transaction.assets.exchange.OrderPriceMode.{AssetDecimals, FixedDecimals}
import vt.assets.exchange.OrderSender

object PBOrders {
  import com.wavesplatform.protobuf.utils.PBImplicitConversions.*

  def vanilla(order: PBOrder): VanillaOrder = {
    VanillaOrder(
      order.version.toByte,
      order.sender match {
        case Sender.SenderPublicKey(value) => OrderSender(PublicKey(value.toByteStr))
        case Sender.Eip712Signature(sig)   => OrderSender.Eip712Signature(sig.toByteStr)
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
      order.proofs.map(_.toByteStr),
      order.priceMode match {
        case DEFAULT if order.version >= 4 => FixedDecimals
        case DEFAULT                       => AssetDecimals
        case ASSET_DECIMALS                => AssetDecimals
        case FIXED_DECIMALS                => FixedDecimals
        case PriceMode.Unrecognized(v)     => throw new IllegalArgumentException(s"Unknown order price mode: $v")
      },
      explicitMode = order.priceMode != DEFAULT
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
        case AssetDecimals if order.explicitMode => ASSET_DECIMALS
        case FixedDecimals if order.explicitMode => FIXED_DECIMALS
        case _                                   => DEFAULT
      },
      order.senderCredentials match {
        case OrderSender.SenderPublicKey(key)       => Sender.SenderPublicKey(key.toByteString)
        case OrderSender.Eip712Signature(signature) => Sender.Eip712Signature(signature.toByteString)
      }
    )
  }
}
