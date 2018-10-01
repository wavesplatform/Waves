package com.wavesplatform.matcher

import com.wavesplatform.account.Address
import com.wavesplatform.database.{RW, ReadOnlyDB}
import com.wavesplatform.matcher.ActiveOrdersIndex._
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.transaction.assets.exchange.Order.Id

class ActiveOrdersIndex(address: Address, elementsLimit: Int) {
  def add(rw: RW, pair: AssetPair, id: Id): Either[String, Unit] = {
    val size = getSize
    if (size >= elementsLimit) Left(s"The maximum orders limit of $size orders has been reached")
    else {
      val newestIdx        = getNewestIdx
      val updatedNewestIdx = newestIdx.getOrElse(0) + 1

      // A new order
      setNode(updatedNewestIdx, Node((pair, id), newestIdx, None))
      setIdx(id, updatedNewestIdx)

      // A previous order in the index
      newestIdx.foreach(updateNode(_, _.copy(newerIdx = Some(updatedNewestIdx))))

      setNewestIdx(updatedNewestIdx)
      setSize(size + 1)
      Right(())
    }
  }

  def delete(rw: RW, id: Id): Unit = {
    val idx  = getIdx(id)
    val node = getNode(idx)

    node.olderIdx.foreach(updateNode(_, _.copy(newerIdx = node.newerIdx)))
    node.newerIdx.foreach(updateNode(_, _.copy(olderIdx = node.olderIdx)))

    if (node.newerIdx.isEmpty) {
      node.olderIdx match {
        case None    => deleteNewestIdx()
        case Some(x) => setNewestIdx(x)
      }
    }

    deleteIdx(id)
    setSize(getSize - 1)
  }

  def iterator(ro: ReadOnlyDB): Iterator[NodeContent] = new NodeIterator(getNewestIdx.map(getNode)).take(elementsLimit).map(_.elem)

  private def getNode(idx: Index): Node                     = ???
  private def setNode(idx: Index, node: Node): Unit         = ???
  private def updateNode(idx: Index, f: Node => Node): Unit = setNode(idx, f(getNode(idx)))

  private def getIdx(id: Id): Int              = ???
  private def setIdx(id: Id, idx: Index): Unit = ???
  private def deleteIdx(id: Id): Unit          = ???

  private def getNewestIdx: Option[Index]    = ???
  private def setNewestIdx(idx: Index): Unit = ???
  private def deleteNewestIdx(): Unit        = ???

  private def getSize: Int                = ???
  private def setSize(newSize: Int): Unit = ???

  private class NodeIterator(private var currNode: Option[Node]) extends Iterator[Node] {
    override def hasNext: Boolean = currNode.nonEmpty
    override def next(): Node = currNode match {
      case Some(r) =>
        currNode = r.olderIdx.map(getNode)
        r
      case None => throw new IllegalStateException("hasNext = false")
    }
  }
}

object ActiveOrdersIndex {
  type NodeContent = (AssetPair, Id)
  type Index       = Int

  private case class Node(elem: NodeContent, olderIdx: Option[Index], newerIdx: Option[Index])
}
