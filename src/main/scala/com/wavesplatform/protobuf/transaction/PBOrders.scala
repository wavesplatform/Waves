package com.wavesplatform.protobuf.transaction
import com.google.protobuf.ByteString
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.assets.exchange.{OrderV1, OrderV2}
import com.wavesplatform.{transaction => vt}

object PBOrders {
  import com.wavesplatform.protobuf.utils.PBInternalImplicits._

  def vanilla(order: PBOrder, version: Int = 0): VanillaOrder = {
    VanillaOrder(
      PublicKeyAccount(order.senderPublicKey.toByteArray),
      PublicKeyAccount(order.matcherPublicKey.toByteArray),
      vt.assets.exchange.AssetPair(Asset.fromProtoId(order.getAssetPair.amountAssetId), Asset.fromProtoId(order.getAssetPair.priceAssetId)),
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
      order.proofs.map(_.toByteArray: ByteStr),
      if (version == 0) order.version.toByte else version.toByte
    )
  }

  def vanillaV1(order: PBOrder): OrderV1 = vanilla(order, 1) match {
    case v1: OrderV1 => v1
    case _           => ???
  }

  def vanillaV2(order: PBOrder): OrderV2 = vanilla(order, 2) match {
    case v1: OrderV2 => v1
    case _           => ???
  }

  def protobuf(order: VanillaOrder): PBOrder = {
    PBOrder(
      chainId = 0,
      ByteString.copyFrom(order.senderPublicKey.publicKey),
      ByteString.copyFrom(order.matcherPublicKey.publicKey),
      Some(PBOrder.AssetPair(order.assetPair.amountAsset.protoId, order.assetPair.priceAsset.protoId)),
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
      order.proofs.map(bs => bs: ByteString)
    )
  }
}
