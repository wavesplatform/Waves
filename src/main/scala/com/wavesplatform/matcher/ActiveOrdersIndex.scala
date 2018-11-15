package com.wavesplatform.matcher

import java.nio.ByteBuffer

import com.google.common.primitives.{Ints, Shorts}
import com.wavesplatform.account.Address
import com.wavesplatform.crypto
import com.wavesplatform.database.{Key, RW, ReadOnlyDB}
import com.wavesplatform.matcher.ActiveOrdersIndex._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.Order.Id

class ActiveOrdersIndex(address: Address, maxElements: Int) {
  def add(rw: RW, pair: AssetPair, id: Id): Unit = {
    val newestIdx        = rw.get(newestIdxKey)
    val updatedNewestIdx = newestIdx.getOrElse(0) + 1

    rw.put(nodeKey(updatedNewestIdx), Some(Node(pair, id)))
    rw.put(orderIdxKey(id), Some(updatedNewestIdx))

    rw.update(sizeKey)(_.orElse(Some(0)).map(_ + 1))
    rw.put(newestIdxKey, Some(updatedNewestIdx))
    if (rw.get(oldestIdxKey).isEmpty) rw.put(oldestIdxKey, Some(updatedNewestIdx))
  }

  def delete(rw: RW, id: Id): Unit = rw.get(orderIdxKey(id)).foreach { idx =>
    val nk = nodeKey(idx)

    rw.delete(nk)
    rw.delete(orderIdxKey(id))

    val newSize = rw.get(sizeKey).getOrElse(0) - 1
    if (newSize <= 0) {
      rw.delete(oldestIdxKey)
      rw.delete(newestIdxKey)
      rw.delete(sizeKey)
    } else {
      rw.put(sizeKey, Some(newSize))
      if (rw.get(oldestIdxKey).contains(idx)) findNewerIdx(rw, idx) match {
        case Some(newestIdx) => rw.put(oldestIdxKey, Some(newestIdx))
        case None            => rw.delete(oldestIdxKey)
      }
    }
  }

  def size(ro: ReadOnlyDB): Int = ro.get(sizeKey).getOrElse(0)

  def getAll(ro: ReadOnlyDB): Vector[Node] = ro.get(oldestIdxKey).fold(Vector.empty[Node]) { oldestIdx =>
    ro.read(MatcherKeys.ActiveOrdersKeyName, prefix, seek = nodeKey(oldestIdx).keyBytes, n = Int.MaxValue)(readNode)
  }

  def has(ro: ReadOnlyDB, id: Id): Boolean = ro.get(orderIdxKey(id)).nonEmpty

  private def findNewerIdx(ro: ReadOnlyDB, thanIdx: Int): Option[Index] = {
    ro.read(MatcherKeys.ActiveOrdersKeyName, prefix, seek = nodeKey(thanIdx + 1).keyBytes, n = 1)(readIdx).headOption
  }

  private def prefix          = MatcherKeys.ActiveOrdersPrefixBytes ++ address.bytes.arr
  private def prefixSize: Int = Shorts.BYTES + Address.AddressLength
  private def readIdx(x: ReadOnlyDB.Entry): Int = {
    val v = x.getKey
    Ints.fromBytes(v(prefixSize), v(prefixSize + 1), v(prefixSize + 2), v(prefixSize + 3))
  }
  private def readNode(x: ReadOnlyDB.Entry): Node = Node.read(x.getValue)

  private val sizeKey: Key[Option[Index]]             = MatcherKeys.activeOrdersSize(address)
  private def nodeKey(idx: Index): Key[Option[Node]]  = MatcherKeys.activeOrders(address, idx)
  private val oldestIdxKey: Key[Option[Index]]        = MatcherKeys.activeOrdersOldestSeqNr(address)
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
