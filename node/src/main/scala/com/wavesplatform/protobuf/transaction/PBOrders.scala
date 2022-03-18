package com.wavesplatform.protobuf.transaction

import cats.syntax.either._
import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf._
import com.wavesplatform.protobuf.order.AssetPair
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.exchange.OrderType
import com.wavesplatform.transaction.{TxExchangeAmount, TxMatcherFee, TxOrderPrice}
import com.wavesplatform.{transaction => vt}

object PBOrders {
  import com.wavesplatform.protobuf.utils.PBImplicitConversions._

  def vanilla(order: PBOrder, version: Int = 0): Either[ValidationError, VanillaOrder] =
    for {
      amount <- TxExchangeAmount.from(order.amount).leftMap(_ => GenericError(TxExchangeAmount.errMsg))
      price <- TxOrderPrice.from(order.price).leftMap(_ => GenericError(TxOrderPrice.errMsg))
      matcherFee <- TxMatcherFee.from(order.getMatcherFee.longAmount).leftMap(_ => GenericError(TxMatcherFee.errMsg))
      orderType <- vanillaOrderType(order.orderSide)
    } yield {
      VanillaOrder(
        if (version == 0) order.version.toByte else version.toByte,
        PublicKey(order.senderPublicKey.toByteArray),
        PublicKey(order.matcherPublicKey.toByteArray),
        vt.assets.exchange
          .AssetPair(PBAmounts.toVanillaAssetId(order.getAssetPair.amountAssetId), PBAmounts.toVanillaAssetId(order.getAssetPair.priceAssetId)),
        orderType,
        amount,
        price,
        order.timestamp,
        order.expiration,
        matcherFee,
        PBAmounts.toVanillaAssetId(order.getMatcherFee.assetId),
        order.proofs.map(_.toByteStr)
      )
    }

  def protobuf(order: VanillaOrder): PBOrder = {
    PBOrder(
      AddressScheme.current.chainId,
      order.senderPublicKey.toByteString,
      order.matcherPublicKey.toByteString,
      Some(AssetPair(PBAmounts.toPBAssetId(order.assetPair.amountAsset), PBAmounts.toPBAssetId(order.assetPair.priceAsset))),
      order.orderType match {
        case vt.assets.exchange.OrderType.BUY  => PBOrder.Side.BUY
        case vt.assets.exchange.OrderType.SELL => PBOrder.Side.SELL
      },
      order.amount.value,
      order.price.value,
      order.timestamp,
      order.expiration,
      Some((order.matcherFeeAssetId, order.matcherFee.value)),
      order.version,
      order.proofs.map(_.toByteString)
    )
  }

  private def vanillaOrderType(orderSide: com.wavesplatform.protobuf.order.Order.Side): Either[GenericError, OrderType] =
    orderSide match {
      case PBOrder.Side.BUY             => Right(vt.assets.exchange.OrderType.BUY)
      case PBOrder.Side.SELL            => Right(vt.assets.exchange.OrderType.SELL)
      case PBOrder.Side.Unrecognized(v) => Left(GenericError(s"Unknown order type: $v"))
    }
}
