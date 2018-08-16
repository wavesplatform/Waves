package com.wavesplatform.matcher.api

import com.wavesplatform.account.Address
import com.wavesplatform.database.{DBExt, RW}
import com.wavesplatform.matcher.MatcherKeys
import com.wavesplatform.matcher.model.OrderInfo
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import org.iq80.leveldb.DB

object DBUtils {
  import OrderInfo.orderStatusOrdering

  def ordersByAddressAndPair(db: DB, address: Address, pair: AssetPair, activeOnly: Boolean): Seq[(Order, OrderInfo)] = db.readOnly { ro =>
    val changedAssets = Set(pair.priceAsset, pair.amountAsset)
    (for {
      idx             <- db.get(MatcherKeys.addressOrdersSeqNr(address)) to 1 by -1
      orderSpendAsset <- ro.get(MatcherKeys.addressOrders(address, idx))
      if changedAssets.isEmpty || changedAssets(orderSpendAsset.spendAsset)
      order <- ro.get(MatcherKeys.order(orderSpendAsset.orderId.arr))
      orderInfo = ro.get(MatcherKeys.orderInfo(orderSpendAsset.orderId.arr))
      if !(activeOnly && orderInfo.status.isFinal) && order.assetPair == pair
    } yield (order, orderInfo)).sortBy { case (order, info) => (info.status, -order.timestamp) }
  }

  def ordersByAddress(db: DB, address: Address, changedAssets: Set[Option[AssetId]], activeOnly: Boolean, maxOrders: Int): Seq[(Order, OrderInfo)] =
    db.readOnly { ro =>
      (for {
        idx             <- (db.get(MatcherKeys.addressOrdersSeqNr(address)) to 1 by -1).view
        orderSpendAsset <- ro.get(MatcherKeys.addressOrders(address, idx))
        if changedAssets.isEmpty || changedAssets(orderSpendAsset.spendAsset)
        order <- ro.get(MatcherKeys.order(orderSpendAsset.orderId.arr))
        orderInfo = ro.get(MatcherKeys.orderInfo(orderSpendAsset.orderId.arr))
        if !(activeOnly && orderInfo.status.isFinal)
      } yield (order, orderInfo)).sortBy { case (order, info) => (info.status, -order.timestamp) }.take(maxOrders)
    }

  def reservedBalance(db: DB, address: Address): Map[Option[AssetId], Long] = db.readOnly { ro =>
    (for {
      idx <- 1 to ro.get(MatcherKeys.openVolumeSeqNr(address))
      assetId = ro.get(MatcherKeys.openVolumeAsset(address, idx))
      volume <- ro.get(MatcherKeys.openVolume(address, assetId))
      if volume != 0
    } yield assetId -> volume).toMap
  }

  def orderInfo(db: DB, orderId: Array[Byte]): OrderInfo            = db.get(MatcherKeys.orderInfo(orderId))
  def orderInfo(rw: RW, orderId: Array[Byte]): OrderInfo            = rw.get(MatcherKeys.orderInfo(orderId))
  def orderInfoOpt(rw: RW, orderId: Array[Byte]): Option[OrderInfo] = rw.get(MatcherKeys.orderInfoOpt(orderId))
}
