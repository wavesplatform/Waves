package com.wavesplatform.matcher.api

import com.wavesplatform.account.Address
import com.wavesplatform.database.{DBExt, RW, ReadOnlyDB}
import com.wavesplatform.matcher._
import com.wavesplatform.matcher.model.OrderInfo
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import org.iq80.leveldb.DB

object DBUtils {
  import OrderInfo.orderStatusOrdering

  object indexes {
    object active {
      val MaxElements = 200

      def add(rw: RW, address: Address, pair: AssetPair, id: Order.Id): Unit = c(address).add(rw, pair, id)
      def delete(rw: RW, address: Address, id: Order.Id): Unit               = c(address).delete(rw, id)
      def size(ro: ReadOnlyDB, address: Address): Int                        = c(address).size(ro)

      // We show all active orders even they count exceeds the pair limit
      def getAll(ro: ReadOnlyDB, address: Address): IndexedSeq[ActiveOrdersIndex.Node] = c(address).getAll(ro)
      def has(ro: ReadOnlyDB, address: Address, id: Order.Id): Boolean                 = c(address).has(ro, id)

      private def c(address: Address) = new ActiveOrdersIndex(address, MaxElements)
    }

    object finalized {
      object common {
        val MaxElements = 100

        def add(rw: RW, address: Address, id: Order.Id): Unit                  = add(rw, address, List(id))
        def add(rw: RW, address: Address, ids: Seq[Order.Id]): Unit            = c(address).add(rw, ids)
        def get(ro: ReadOnlyDB, address: Address, nNewest: Int): Seq[Order.Id] = c(address).get(ro, nNewest)

        private def c(address: Address) = new FinalizedOrdersCommonIndex(address, MaxElements)
      }

      object pair {
        val MaxElements = 100

        def add(rw: RW, address: Address, pair: AssetPair, id: Order.Id): Unit                  = add(rw, address, pair, List(id))
        def add(rw: RW, address: Address, pair: AssetPair, ids: Seq[Order.Id]): Unit            = c(address, pair).add(rw, ids)
        def get(ro: ReadOnlyDB, address: Address, pair: AssetPair, nNewest: Int): Seq[Order.Id] = c(address, pair).get(ro, nNewest)

        private def c(address: Address, pair: AssetPair) = new FinalizedOrdersPairIndex(address, pair, MaxElements)
      }
    }
  }

  def ordersByAddressAndPair(db: DB, address: Address, pair: AssetPair, activeOnly: Boolean, maxOrders: Int): IndexedSeq[(Order, OrderInfo)] =
    db.readOnly { ro =>
      mergeOrders(
        ro,
        maxOrders,
        activeOnly,
        active = indexes.active.getAll(ro, address).collect { case x if x.pair == `pair` => x.id },
        getFinalizedIndex = indexes.finalized.pair.get(ro, address, pair, _)
      )
    }

  /**
    * @param activeOnly If false - returns all active orders and the (maxOrders - allActiveOrders.size) recent of others
    */
  def ordersByAddress(db: DB, address: Address, activeOnly: Boolean, maxOrders: Int): IndexedSeq[(Order, OrderInfo)] = db.readOnly { ro =>
    mergeOrders(
      ro,
      maxOrders,
      activeOnly,
      active = indexes.active.getAll(ro, address).map(_.id),
      getFinalizedIndex = indexes.finalized.common.get(ro, address, _)
    )
  }

  private def mergeOrders(ro: ReadOnlyDB,
                          maxOrders: Int,
                          activeOnly: Boolean,
                          active: IndexedSeq[Order.Id],
                          getFinalizedIndex: Int => Seq[Order.Id]): IndexedSeq[(Order, OrderInfo)] = {
    def get(id: Order.Id): (Option[Order], Option[OrderInfo]) = (ro.get(MatcherKeys.order(id)), ro.get(MatcherKeys.orderInfoOpt(id)))

    val finalized = if (activeOnly) Seq.empty else getFinalizedIndex(maxOrders - active.size)
    (active ++ finalized)
      .map(get)
      .collect { case (Some(o), Some(oi)) => (o, oi) }
      .sortBy { case (order, info) => (info.status, -order.timestamp) }
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

  def transactionsForOrder(db: DB, orderId: ByteStr): Seq[ExchangeTransaction] = db.readOnly { ro =>
    for {
      seqNr <- 1 to ro.get(MatcherKeys.orderTxIdsSeqNr(orderId))
      txId = ro.get(MatcherKeys.orderTxId(orderId, seqNr))
      tx <- ro.get(MatcherKeys.exchangeTransaction(txId))
    } yield tx
  }

  def openVolume(db: DB, address: Address, assetId: Option[AssetId]): Long = db.get(MatcherKeys.openVolume(address, assetId)).getOrElse(0L)
  def activeOrderCount(db: DB, address: Address): Int = {
    val key = MatcherKeys.activeOrdersSize(address)
    key.parse(db.get(key.keyBytes)).getOrElse(0)
  }

  def lastOrderTimestamp(db: DB, address: Address): Option[Long] = db.get(MatcherKeys.lastCommandTimestamp(address))
}
