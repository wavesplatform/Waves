package com.wavesplatform.matcher.api

import com.wavesplatform.account.Address
import com.wavesplatform.database.{DBExt, RW}
import com.wavesplatform.matcher.MatcherKeys
import com.wavesplatform.matcher.model.OrderInfo
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import org.iq80.leveldb.DB

object DBUtils {
  import OrderInfo.orderStatusOrdering

  def ordersByAddressAndPair(db: DB, address: Address, pair: AssetPair, maxOrders: Int): Seq[(Order, OrderInfo)] = db.readOnly { ro =>
    (for {
      idx     <- (db.get(MatcherKeys.addressOrdersByPairSeqNr(address, pair)) to 0 by -1).view
      orderId <- ro.get(MatcherKeys.addressOrdersByPair(address, pair, idx))
      order   <- ro.get(MatcherKeys.order(orderId))
      orderInfo = ro.get(MatcherKeys.orderInfo(orderId))
    } yield (order, orderInfo)).take(maxOrders).sortBy { case (order, info) => (info.status, -order.timestamp) }
  }

  /**
    * @param activeOnly If false - returns all active orders and the (maxOrders - allActiveOrders.size) recent of others
    */
  def ordersByAddress(db: DB, address: Address, activeOnly: Boolean, maxOrders: Int): Seq[(Order, OrderInfo)] =
    db.readOnly { ro =>
      val oldestActiveIdx = db.get(MatcherKeys.addressOldestActiveOrderSeqNr(address))
      val lastIdx         = db.get(MatcherKeys.addressOrdersSeqNr(address))

      val orders = for {
        idx             <- (lastIdx to 1 by -1).toStream
        orderSpendAsset <- ro.get(MatcherKeys.addressOrders(address, idx))
        orderInfo       <- ro.get(MatcherKeys.orderInfoOpt(orderSpendAsset.orderId))
      } yield (orderSpendAsset.orderId, orderInfo)

      val activeNumber        = oldestActiveIdx.fold(0)(x => math.min(lastIdx - x + 1, maxOrders))
      val (finalized, active) = orders.partition(_._2.status.isFinal)
      val activeEager         = active.take(activeNumber).force
      val finalizedEager      = if (activeOnly) Stream.empty else finalized.take(maxOrders - activeEager.size).force

      val r = for {
        x     <- activeEager ++ finalizedEager
        order <- ro.get(MatcherKeys.order(x._1))
      } yield (order, x._2)

      r.toVector.sortBy { case (order, info) => (info.status, -order.timestamp) }
    }

  def reservedBalance(db: DB, address: Address): Map[Option[AssetId], Long] = db.readOnly { ro =>
    (for {
      idx <- 1 to ro.get(MatcherKeys.openVolumeSeqNr(address))
      assetId = ro.get(MatcherKeys.openVolumeAsset(address, idx))
      volume <- ro.get(MatcherKeys.openVolume(address, assetId))
      if volume != 0
    } yield assetId -> volume).toMap
  }

  def order(db: DB, orderId: ByteStr): Option[Order] = db.get(MatcherKeys.order(orderId))

  def orderInfo(db: DB, orderId: ByteStr): OrderInfo = db.get(MatcherKeys.orderInfo(orderId))
  def orderInfo(rw: RW, orderId: ByteStr): OrderInfo = rw.get(MatcherKeys.orderInfo(orderId))
}
