package com.wavesplatform.matcher

import com.wavesplatform.account.Address
import com.wavesplatform.database.{Key, RW, ReadOnlyDB}
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.Order.Id

class FinalizedOrdersCommonIndex(address: Address, elementsLimit: Int) extends FinalizedOrdersIndex(elementsLimit, deleteOutdatedOrders = false) {
  override protected def itemKey(idx: Index): Key[Option[Id]] = MatcherKeys.finalizedCommon(address, idx)
  override protected def newestKey: Key[Option[Index]]        = MatcherKeys.finalizedCommonSeqNr(address)
  override protected def prefix: Array[Byte]                  = MatcherKeys.FinalizedCommonPrefixBytes ++ address.bytes.arr
}

class FinalizedOrdersPairIndex(address: Address, pair: AssetPair, elementsLimit: Int)
    extends FinalizedOrdersIndex(elementsLimit, deleteOutdatedOrders = true) {
  override protected def itemKey(idx: Index): Key[Option[Id]] = MatcherKeys.finalizedPair(address, pair, idx)
  override protected def newestKey: Key[Option[Index]]        = MatcherKeys.finalizedPairSeqNr(address, pair)
  override protected def prefix: Array[Byte]                  = MatcherKeys.FinalizedPairPrefixBytes ++ address.bytes.arr ++ pair.bytes
}

abstract class FinalizedOrdersIndex(elementsLimit: Int, deleteOutdatedOrders: Boolean) {
  type Index = Int

  def add(rw: RW, ids: Seq[Id]): Unit = if (ids.nonEmpty) {
    val origNewestIdxOpt = rw.get(newestKey)

    val newItems = ids.zip((origNewestIdxOpt.getOrElse(0) + 1) to Int.MaxValue)
    newItems.foreach { case (id, idx) => rw.put(itemKey(idx), Some(id)) }

    val updatedNewestIdx = newItems.last._2
    (origNewestIdxOpt.getOrElse(1) to updatedNewestIdx).dropRight(elementsLimit).foreach { idx =>
      val k = itemKey(idx)
      if (deleteOutdatedOrders) rw.get(k).foreach { id =>
        rw.delete(MatcherKeys.order(id))
        rw.delete(MatcherKeys.orderInfo(id))
      }
      rw.delete(k)
    }
    rw.put(newestKey, Some(updatedNewestIdx))
  }

  def get(ro: ReadOnlyDB, nNewest: Int): Seq[Id] = {
    ro.get(newestKey).fold(Seq.empty[Id]) { newestIdx =>
      val fromIdx = math.max(newestIdx - nNewest + 1, 1)
      ro.read[Id](prefix, seek = itemKey(fromIdx).keyBytes, n = math.min(newestIdx, nNewest))(x => ByteStr(x.getValue))
    }
  }

  protected def itemKey(idx: Index): Key[Option[Id]]
  protected def newestKey: Key[Option[Index]]
  protected def prefix: Array[Byte]
}
