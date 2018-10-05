package com.wavesplatform.matcher

import java.nio.ByteBuffer

import com.google.common.primitives.Ints
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

    // A new order
    rw.put(nodeKey(updatedNewestIdx), Node(newestIdx, (pair, id), None))
    rw.put(orderIdxKey(id), Some(updatedNewestIdx))

    // A previous order in the index
    newestIdx.foreach(idx => rw.update(nodeKey(idx))(_.copy(newerIdx = Some(updatedNewestIdx))))

    rw.put(newestIdxKey, Some(updatedNewestIdx))
    rw.update(sizeKey) { orig =>
      val size = orig.getOrElse(0)
      Some(size + 1)
    }
  }

  def delete(rw: RW, id: Id): Unit = rw.get(orderIdxKey(id)).foreach { idx =>
    val node = rw.get(nodeKey(idx))

    node.olderIdx.foreach(idx => rw.update(nodeKey(idx))(_.copy(newerIdx = node.newerIdx)))
    node.newerIdx.foreach(idx => rw.update(nodeKey(idx))(_.copy(olderIdx = node.olderIdx)))

    if (node.newerIdx.isEmpty) {
      node.olderIdx match {
        case None => rw.delete(newestIdxKey)
        case x    => rw.put(newestIdxKey, x)
      }
    }

    rw.delete(orderIdxKey(id))
    rw.get(sizeKey).getOrElse(0) - 1 match {
      case x if x <= 0 => rw.delete(sizeKey)
      case x           => rw.put(sizeKey, Some(x))
    }
  }

  def size(ro: ReadOnlyDB): Int = ro.get(sizeKey).getOrElse(0)

  def iterator(ro: ReadOnlyDB): Iterator[NodeContent] = {
    val newestIdx = ro.get(newestIdxKey)
    val latest    = newestIdx.map(idx => ro.get(nodeKey(idx)))
    new NodeIterator(ro, latest)
      .take(math.min(maxElements, ro.get(sizeKey).getOrElse(0)))
      .map(_.elem)
  }

  private val sizeKey: Key[Option[Index]]             = MatcherKeys.activeOrdersSize(address)
  private def nodeKey(idx: Index): Key[Node]          = MatcherKeys.activeOrders(address, idx)
  private val newestIdxKey: Key[Option[Index]]        = MatcherKeys.activeOrdersSeqNr(address)
  private def orderIdxKey(id: Id): Key[Option[Index]] = MatcherKeys.activeOrderSeqNr(address, id)

  private class NodeIterator(ro: ReadOnlyDB, private var currNode: Option[Node]) extends Iterator[Node] {
    override def hasNext: Boolean = currNode.nonEmpty
    override def next(): Node = currNode match {
      case Some(r) =>
        currNode = r.olderIdx.map(idx => ro.get(nodeKey(idx)))
        r
      case None => throw new IllegalStateException("hasNext = false")
    }
  }
}

object ActiveOrdersIndex {
  type NodeContent = (AssetPair, Id)
  type Index       = Int

  // TODO: We could read the previous and next records by iterator, so newerIdx is not required
  case class Node(olderIdx: Option[Index], elem: (AssetPair, Id), newerIdx: Option[Index])
  object Node {
    def read(xs: Array[Byte]): Node = {
      val bb = ByteBuffer.wrap(xs)
      Node(indexFromBytes(bb), (assetPairFromBytes(bb), orderIdFromBytes(bb)), indexFromBytes(bb))
    }

    private def indexFromBytes(bb: ByteBuffer): Option[Index] = bb.get match {
      case 0 => None
      case 1 =>
        val bytes = new Array[Byte](4)
        bb.get(bytes)
        Some(Ints.fromByteArray(bytes))
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

    def write(x: Node): Array[Byte] = indexToBytes(x.olderIdx) ++ toBytes(x.elem) ++ indexToBytes(x.newerIdx)

    private def indexToBytes(x: Option[Index]): Array[Byte] = x.fold(Array[Byte](0))(x => (1: Byte) +: Ints.toByteArray(x))
    private def toBytes(x: (AssetPair, Id)): Array[Byte]    = x._1.bytes ++ x._2.arr
  }
}
