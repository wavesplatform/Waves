package com.wavesplatform.matcher.api

import com.wavesplatform.account.Address
import com.wavesplatform.database.{DBExt, RW, ReadOnlyDB}
import com.wavesplatform.matcher.model.OrderInfo
import com.wavesplatform.matcher.{ActiveOrdersIndex, FinalizedOrdersCommonIndex, FinalizedOrdersPairIndex, MatcherKeys}
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import org.iq80.leveldb.DB

object DBUtils {
  import OrderInfo.orderStatusOrdering

  object indexes {
    object active {
      val MaxElements = 200

      def add(rw: RW, address: Address, pair: AssetPair, id: Order.Id): Unit                  = c(address).add(rw, pair, id)
      def delete(rw: RW, address: Address, id: Order.Id): Unit                                = c(address).delete(rw, id)
      def size(ro: ReadOnlyDB, address: Address): Int                                         = c(address).size(ro)
      def iterator(ro: ReadOnlyDB, address: Address): Iterator[ActiveOrdersIndex.NodeContent] = c(address).iterator(ro)

      private def c(address: Address) = new ActiveOrdersIndex(address, MaxElements)
    }

    object finalized {
      object common {
        def add(rw: RW, address: Address, id: Order.Id): Unit              = add(rw, address, List(id))
        def add(rw: RW, address: Address, ids: Seq[Order.Id]): Unit        = c(address).add(rw, ids)
        def iterator(ro: ReadOnlyDB, address: Address): Iterator[Order.Id] = c(address).iterator(ro)

        private def c(address: Address) = new FinalizedOrdersCommonIndex(address, 100)
      }

      object pair {
        def add(rw: RW, address: Address, pair: AssetPair, id: Order.Id): Unit              = add(rw, address, pair, List(id))
        def add(rw: RW, address: Address, pair: AssetPair, ids: Seq[Order.Id]): Unit        = c(address, pair).add(rw, ids)
        def iterator(ro: ReadOnlyDB, address: Address, pair: AssetPair): Iterator[Order.Id] = c(address, pair).iterator(ro)

        private def c(address: Address, pair: AssetPair) = new FinalizedOrdersPairIndex(address, pair, 100)
      }
    }
  }

  def ordersByAddressAndPair(db: DB, address: Address, pair: AssetPair, maxOrders: Int): IndexedSeq[(Order, OrderInfo)] = db.readOnly { ro =>
    mergeOrders(
      ro,
      maxOrders,
      activeIndex = indexes.active.iterator(ro, address).collect { case (`pair`, id) => id },
      finalizedIndex = indexes.finalized.pair.iterator(ro, address, pair)
    )
  }

  /**
    * @param activeOnly If false - returns all active orders and the (maxOrders - allActiveOrders.size) recent of others
    */
  def ordersByAddress(db: DB, address: Address, activeOnly: Boolean, maxOrders: Int): IndexedSeq[(Order, OrderInfo)] = db.readOnly { ro =>
    mergeOrders(
      ro,
      maxOrders,
      activeIndex = indexes.active.iterator(ro, address).map { case (_, id) => id },
      finalizedIndex = if (activeOnly) Iterator.empty else indexes.finalized.common.iterator(ro, address)
    )
  }

  private def mergeOrders(ro: ReadOnlyDB,
                          maxOrders: Int,
                          activeIndex: Iterator[Order.Id],
                          finalizedIndex: Iterator[Order.Id]): IndexedSeq[(Order, OrderInfo)] = {
    def get(id: Order.Id): (Option[Order], Option[OrderInfo]) = (ro.get(MatcherKeys.order(id)), ro.get(MatcherKeys.orderInfoOpt(id)))

    val active = activeIndex.take(maxOrders).map(get).collect { case (Some(o), Some(oi)) => (o, oi) }.toIndexedSeq

    val nonActive = finalizedIndex
      .map(get)
      .collect { case (Some(o), Some(oi)) => (o, oi) }
      .take(maxOrders - active.size)
      .toVector

    (active ++ nonActive).sortBy { case (order, info) => (info.status, -order.timestamp) }
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
