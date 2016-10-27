package scorex.transaction.state.database.blockchain

import scala.util.Try
import com.google.common.primitives.Ints
import org.h2.mvstore.{MVMap, MVStore}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.{Order, OrderMatch}
import scorex.utils.LogMVMapBuilder

trait OrderMatchStoredState {
  val db: MVStore

  val OrderMatchTx = "OrderMatchTx"

  /**
    * Order id -> serialized OrderMatch transactions
    */
  private val orderMatchTx: MVMap[String, Array[Array[Byte]]] =
    db.openMap(OrderMatchTx, new LogMVMapBuilder[String, Array[Array[Byte]]])

  def putOrderMatch(om: OrderMatch, height: Int): Unit = {
    def appendBytesIfAbsent(prev: Array[Array[Byte]], bytes: Array[Byte])  = {
      if (!prev.exists(_ sameElements bytes)) {
        prev :+ bytes
      } else prev
    }
    def putByOrderId(id: Array[Byte]) = {
      val idStr = Base58.encode(id)
      val prev = Option(orderMatchTx.get(idStr)).getOrElse(Array[Array[Byte]]())
      orderMatchTx.put(idStr, appendBytesIfAbsent(prev, Ints.toByteArray(height) ++ om.bytes))
    }
    putByOrderId(om.buyOrder.id)
    putByOrderId(om.sellOrder.id)
  }

  private val emptyTxSeq = Array(Array[Byte]())

  private def parseTxSeq(aa: Array[Array[Byte]]): Set[OrderMatch] = {
    aa.flatMap { b =>
      OrderMatch.parseBytes(b.drop(Ints.BYTES)).toOption
    }.toSet
  }

  def findPrevOrderMatchTxs(om: OrderMatch): Set[OrderMatch] = {
    findPrevOrderMatchTxs(om.buyOrder) ++ findPrevOrderMatchTxs(om.sellOrder)
  }

  def findPrevOrderMatchTxs(order: Order): Set[OrderMatch] = {
    parseTxSeq(Option(orderMatchTx.get(Base58.encode(order.id))).getOrElse(emptyTxSeq))
  }

  def rollbackOrderMatchTo(rollbackTo: Int): Unit = {
    //orderMatchTx.keySet().forEach()
  }
}
