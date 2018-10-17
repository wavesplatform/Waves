package com.wavesplatform.matcher

import com.wavesplatform.account.Address
import com.wavesplatform.database.{Key, RW, ReadOnlyDB}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.Order.Id

class FinalizedOrdersCommonIndex(address: Address, elementsLimit: Int) extends FinalizedOrdersIndex(elementsLimit) {
  override protected def itemKey(idx: Index): Key[Option[Id]] = MatcherKeys.finalizedCommon(address, idx)
  override protected def newestKey: Key[Option[Index]]        = MatcherKeys.finalizedCommonSeqNr(address)
  override protected val deleteOutdatedOrders: Boolean        = false
}

class FinalizedOrdersPairIndex(address: Address, pair: AssetPair, elementsLimit: Int) extends FinalizedOrdersIndex(elementsLimit) {
  override protected def itemKey(idx: Index): Key[Option[Id]] = MatcherKeys.finalizedPair(address, pair, idx)
  override protected def newestKey: Key[Option[Index]]        = MatcherKeys.finalizedPairSeqNr(address, pair)
  override protected val deleteOutdatedOrders: Boolean        = true
}

abstract class FinalizedOrdersIndex(elementsLimit: Int) {
  type Index = Int

  def add(rw: RW, ids: Seq[Id]): Unit = if (ids.nonEmpty) {
    val newestIdx = rw.get(newestKey).getOrElse(0)
    ids.zipWithIndex.foreach {
      case (id, offset) =>
        val idx = newestIdx + 1 + offset
        rw.put(itemKey(idx), Some(id))
    }

    val size             = ids.size
    val updatedNewestIdx = newestIdx + size

    if (deleteOutdatedOrders) {
      val oldSize   = math.min(newestIdx, elementsLimit)
      val totalSize = oldSize + size
      (updatedNewestIdx to 1 by -1).slice(elementsLimit, totalSize).foreach { idx =>
        val k = itemKey(idx)
        rw.get(k).foreach { id =>
          rw.delete(MatcherKeys.order(id))
          rw.delete(MatcherKeys.orderInfo(id))
        }
        rw.delete(k)
      }
    }

    rw.put(newestKey, Some(updatedNewestIdx))
  }

  def iterator(ro: ReadOnlyDB): Iterator[Id] = {
    val newestIdx = ro.get(newestKey)
    newestIdx.fold(Iterator.empty: Iterator[Id]) { newestIdx =>
      (newestIdx to 1 by -1).iterator.take(elementsLimit).flatMap(idx => ro.get(itemKey(idx)))
    }
  }

  protected def itemKey(idx: Index): Key[Option[Id]]
  protected def newestKey: Key[Option[Index]]
  protected val deleteOutdatedOrders: Boolean
}
