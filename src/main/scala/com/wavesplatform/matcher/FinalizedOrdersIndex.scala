package com.wavesplatform.matcher

import java.nio.ByteBuffer

import cats.syntax.functor._
import com.google.common.primitives.Shorts
import com.wavesplatform.account.Address
import com.wavesplatform.database.{Key, RW, ReadOnlyDB}
import com.wavesplatform.db.prefixBackwardIterator
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.Order.Id

class FinalizedOrdersCommonIndex(address: Address, elementsLimit: Int) extends FinalizedOrdersIndex(elementsLimit) {
  override protected def itemKey(idx: Index): Key[Option[Id]] = MatcherKeys.finalizedCommon(address, idx)
  override protected def newestKey: Key[Option[Index]]        = MatcherKeys.finalizedCommonSeqNr(address)
  override protected def prefix: Array[Byte]                  = Shorts.toByteArray(14) ++ address.bytes.arr
}

class FinalizedOrdersPairIndex(address: Address, pair: AssetPair, elementsLimit: Int) extends FinalizedOrdersIndex(elementsLimit) {
  override protected def itemKey(idx: Index): Key[Option[Id]] = MatcherKeys.finalizedPair(address, pair, idx)
  override protected def newestKey: Key[Option[Index]]        = MatcherKeys.finalizedPairSeqNr(address, pair)
  override protected def prefix: Array[Byte]                  = Shorts.toByteArray(16) ++ address.bytes.arr ++ pair.bytes
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

    (updatedNewestIdx to 1).slice(elementsLimit, elementsLimit + size).foreach { idx =>
      val k = itemKey(idx)
      rw.get(k).foreach { id =>
        rw.delete(MatcherKeys.order(id))
        rw.delete(MatcherKeys.orderInfo(id))
      }
      rw.delete(k)
    }

    rw.put(newestKey, Some(updatedNewestIdx))
  }

  def iterator(ro: ReadOnlyDB): ClosableIterable[Id] =
    ro.get(newestKey).fold(ClosableIterable.empty: ClosableIterable[Id])(safeIterator(ro, _).map(_._2))

  private def safeIterator(ro: ReadOnlyDB, fromIdx: Int): ClosableIterable[(Index, Id)] = new ClosableIterable[(Index, Id)] {
    private val internal = ro.iterator
    internal.seek(itemKey(fromIdx).keyBytes)

    override val iterator: Iterator[(Index, Id)] = prefixBackwardIterator(internal, prefix) { e =>
      val idx = ByteBuffer.wrap(e.getKey).position(prefix.length).asInstanceOf[ByteBuffer].getInt()
      (idx, ByteStr(e.getValue))
    }

    override def close(): Unit = internal.close()
  }

  protected def itemKey(idx: Index): Key[Option[Id]]
  protected def newestKey: Key[Option[Index]]
  protected def prefix: Array[Byte]
}
