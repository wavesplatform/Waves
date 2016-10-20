package scorex.transaction.state.database.blockchain

import scala.util.Try

import org.h2.mvstore.{MVMap, MVStore}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.OrderMatch
import scorex.utils.LogMVMapBuilder

trait OrderMatchStoredState {
  val db: MVStore

  val OrderMatchTx = "OrderMatchTx"

  /**
    * Order id -> serialized OrderMatch transactions
    */
  private val orderMatchTx: MVMap[String, Array[Array[Byte]]] =
    db.openMap(OrderMatchTx, new LogMVMapBuilder[String, Array[Array[Byte]]])

  def putOrderMatch(om: OrderMatch): Unit = {
    def appendBytesIfAbsent(prev: Array[Array[Byte]], bytes: Array[Byte])  = {
      if (!prev.exists(_ sameElements bytes)) {
        prev :+ bytes
      } else prev
    }
    def putByOrderId(id: Array[Byte]) = {
      val idStr = Base58.encode(id)
      val prev = Option(orderMatchTx.get(idStr)).getOrElse(Array[Array[Byte]]())
      orderMatchTx.put(idStr, appendBytesIfAbsent(prev, om.bytes))
    }
    putByOrderId(om.buyOrder.id)
    putByOrderId(om.sellOrder.id)
  }

  def findPrevOrderMatchTxs(om: OrderMatch): Set[OrderMatch] = {
    val id1 = Base58.encode(om.buyOrder.id)
    val id2 = Base58.encode(om.sellOrder.id)
    (Option(orderMatchTx.get(id1)).getOrElse(Array(Array[Byte]())).flatMap(OrderMatch.parseBytes(_).toOption) ++
      Option(orderMatchTx.get(id2)).getOrElse(Array(Array[Byte]())).flatMap(OrderMatch.parseBytes(_).toOption)).toSet
  }
}
