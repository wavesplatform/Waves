package com.wavesplatform.matcher

import com.wavesplatform.account.Address
import com.wavesplatform.database.{Key, RW, ReadOnlyDB}
import com.wavesplatform.db.prefixIterator
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

    // to 1 because seqs cannot contain more than Int.MaxValue elements
    val newItems = ids.zip(origNewestIdxOpt.fold(Int.MaxValue)(_ - 1) to 1 by -1)
    newItems.foreach { case (id, idx) => rw.put(itemKey(idx), Some(id)) }

    val updatedNewestIdx = newItems.last._2
    (updatedNewestIdx to origNewestIdxOpt.getOrElse(Int.MaxValue)).drop(elementsLimit).foreach { idx =>
      val k = itemKey(idx)
      rw.delete(k)
      if (deleteOutdatedOrders) rw.get(k).foreach { id =>
        rw.delete(MatcherKeys.order(id))
        rw.delete(MatcherKeys.orderInfo(id))
      }
    }

    rw.put(newestKey, Some(updatedNewestIdx))
  }

  def iterator(ro: ReadOnlyDB): ClosableIterable[Id] =
    ro.get(newestKey).fold(ClosableIterable.empty: ClosableIterable[Id])(mkIterator(ro, _))

  private def mkIterator(ro: ReadOnlyDB, latestIdx: Int): ClosableIterable[Id] = new ClosableIterable[Id] {
    private val internal = ro.iterator
    internal.seek(itemKey(latestIdx).keyBytes)

    override val iterator: Iterator[Id] = prefixIterator(internal, prefix)(e => ByteStr(e.getValue))

    override def close(): Unit = internal.close()
  }

  protected def itemKey(idx: Index): Key[Option[Id]]
  protected def newestKey: Key[Option[Index]]
  protected def prefix: Array[Byte]
}
