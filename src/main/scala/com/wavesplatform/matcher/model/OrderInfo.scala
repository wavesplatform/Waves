package com.wavesplatform.matcher.model

import java.nio.ByteBuffer

import com.wavesplatform.matcher.util.Codecs.ByteBufferExt
import com.wavesplatform.transaction.assets.exchange.{AssetPair, OrderType}

case class OrderInfo(side: OrderType, amount: Long, price: Long, timestamp: Long, status: LimitOrder.OrderStatus, assetPair: AssetPair)

object OrderInfo {
  def encode(oi: OrderInfo): Array[Byte] = {
    val assetPairBytes = oi.assetPair.bytes
    ByteBuffer
      .allocate(34 + assetPairBytes.length)
      .put(oi.side.bytes)
      .putLong(oi.amount)
      .putLong(oi.price)
      .putLong(oi.timestamp)
      .putFinalOrderStatus(oi.status)
      .putAssetId(oi.assetPair.amountAsset)
      .putAssetId(oi.assetPair.priceAsset)
      .array()
  }

  def decode(bytes: Array[Byte]): OrderInfo = {
    val buf = ByteBuffer.wrap(bytes)
    OrderInfo(OrderType(buf.get()), buf.getLong, buf.getLong, buf.getLong, buf.getFinalOrderStatus, AssetPair(buf.getAssetId, buf.getAssetId))
  }
}
