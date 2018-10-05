package com.wavesplatform.matcher

import java.nio.ByteBuffer

import com.google.common.primitives.{Ints, Shorts}
import com.wavesplatform.account.Address
import com.wavesplatform.crypto
import com.wavesplatform.database.{Key, RW, ReadOnlyDB}
import com.wavesplatform.db.prefixIterator
import com.wavesplatform.matcher.ActiveOrdersIndex._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.Order.Id

class ActiveOrdersIndex(address: Address, maxElements: Int) {
  private type RawItem = java.util.Map.Entry[Array[Byte], Array[Byte]]

  def add(rw: RW, pair: AssetPair, id: Id): Unit = {
    val newestIdx        = rw.get(newestIdxKey)
    val updatedNewestIdx = newestIdx.fold(Int.MaxValue)(_ - 1)

    rw.put(nodeKey(updatedNewestIdx), Node(pair, id))
    rw.put(orderIdxKey(id), Some(updatedNewestIdx))

    rw.put(newestIdxKey, Some(updatedNewestIdx))
    rw.update(sizeKey) { orig =>
      val size = orig.getOrElse(0)
      Some(size + 1)
    }
  }

  def delete(rw: RW, id: Id): Unit = rw.get(orderIdxKey(id)).foreach { idx =>
    val nk = nodeKey(idx)

    rw.delete(nk)
    rw.delete(orderIdxKey(id))

    val newSize = rw.get(sizeKey).getOrElse(0) - 1
    if (newSize <= 0) {
      rw.delete(newestIdxKey)
      rw.delete(sizeKey)
    } else {
      rw.put(sizeKey, Some(newSize))
      if (rw.get(newestIdxKey).contains(idx)) findOlderIdx(rw, idx).foreach { newestIdx =>
        rw.put(newestIdxKey, Some(newestIdx))
      }
    }
  }

  def size(ro: ReadOnlyDB): Int = ro.get(sizeKey).getOrElse(0)

  def iterator(ro: ReadOnlyDB): ClosableIterable[Node] =
    ro.get(newestIdxKey).fold(ClosableIterable.empty: ClosableIterable[Node])(mkIterator(ro, _)(readNode))

  private def findOlderIdx(ro: ReadOnlyDB, thanIdx: Int): Option[Index] = {
    val iter = mkIterator(ro, latestIdx = thanIdx)(readIdx)
    try iter.iterator.drop(1).find(_ => true)
    finally iter.close()
  }

  private def prefixSize: Int = Shorts.BYTES + Address.AddressLength
  private def readIdx(x: RawItem): Int = {
    val v = x.getKey
    Ints.fromBytes(v(prefixSize), v(prefixSize + 1), v(prefixSize + 2), v(prefixSize + 3))
  }
  private def readNode(x: RawItem): Node = Node.read(x.getValue)

  private def mkIterator[T](ro: ReadOnlyDB, latestIdx: Int)(deserialize: RawItem => T): ClosableIterable[T] =
    new ClosableIterable[T] {
      private val prefix   = MatcherKeys.ActiveOrdersPrefixBytes ++ address.bytes.arr
      private val internal = ro.iterator
      internal.seek(nodeKey(latestIdx).keyBytes)

      override val iterator: Iterator[T] = prefixIterator(internal, prefix)(deserialize)
      override def close(): Unit         = internal.close()
    }

  private val sizeKey: Key[Option[Index]]             = MatcherKeys.activeOrdersSize(address)
  private def nodeKey(idx: Index): Key[Node]          = MatcherKeys.activeOrders(address, idx)
  private val newestIdxKey: Key[Option[Index]]        = MatcherKeys.activeOrdersSeqNr(address)
  private def orderIdxKey(id: Id): Key[Option[Index]] = MatcherKeys.activeOrderSeqNr(address, id)
}

object ActiveOrdersIndex {
  type Index = Int

  case class Node(pair: AssetPair, id: Id)
  object Node {
    def read(xs: Array[Byte]): Node = {
      val bb = ByteBuffer.wrap(xs)
      Node(assetPairFromBytes(bb), orderIdFromBytes(bb))
    }

    private def assetPairFromBytes(bb: ByteBuffer): AssetPair = AssetPair(assetIdFromBytes(bb), assetIdFromBytes(bb))
    private def assetIdFromBytes(bb: ByteBuffer): Option[AssetId] = bb.get match {
      case 0 => None
      case 1 =>
        val bytes = new Array[Byte](crypto.DigestSize)
        bb.get(bytes)
        Some(ByteStr(bytes))
    }

    private def orderIdFromBytes(bb: ByteBuffer): Id = {
      val bytes = new Array[Byte](crypto.DigestSize)
      bb.get(bytes)
      ByteStr(bytes)
    }

    def write(x: Node): Array[Byte] = x.pair.bytes ++ x.id.arr
  }
}
