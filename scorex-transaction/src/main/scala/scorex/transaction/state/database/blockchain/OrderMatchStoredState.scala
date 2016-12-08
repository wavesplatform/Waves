package scorex.transaction.state.database.blockchain

import org.h2.mvstore.{MVMap, MVStore}
import scorex.crypto.encode.Base58
import scorex.transaction.Transaction
import scorex.transaction.assets.exchange.{Order, OrderCancelTransaction, OrderMatch}

import scala.collection.JavaConversions._
import scala.collection.mutable

trait OrderMatchStoredState {
  val db: MVStore
  val transactionsMap: MVMap[Array[Byte], Array[Byte]]

  val OrderMatchTx = "OrderMatchTx"
  val OrderMatchDays = "OrderMatchSavedDays"
  val OrderToCancelTxName = "OrderToCancelTx"
  val MaxLiveDays = (Order.MaxLiveTime / 24L * 60L * 60L * 1000L).toInt

  /**
    * Returns Map Order id -> OrderMatch transactions Ids by Timestamp - starting of the day
    */
  private def orderMatchTxByDay(orderTimestamp: Long): MVMap[String, Array[String]] =
    db.openMap(OrderMatchTx + orderTimestamp)

  private val savedDays: MVMap[Long, Boolean] = db.openMap(OrderMatchDays)

  /**
    * Returns Map OrderId -> OrderCancelTransaction Id
    */
  private def orderToCancelTx: MVMap[String, Array[Byte]] = db.openMap(OrderToCancelTxName)

  def putOrderMatch(om: OrderMatch, blockTs: Long): Unit = {
    def isSaveNeeded(order: Order): Boolean = {
      order.maxTimestamp >= blockTs
    }

    def putOrder(order: Order) = {
      if (isSaveNeeded(order)) {
        val orderDay = calcStartDay(order.maxTimestamp)
        savedDays.put(orderDay, true)
        val orderIdStr = Base58.encode(order.id)
        val omIdStr = Base58.encode(om.id)
        val m = orderMatchTxByDay(orderDay)
        val prev = Option(m.get(orderIdStr)).getOrElse(Array.empty[String])
        if (!prev.contains(omIdStr)) m.put(orderIdStr, prev :+ omIdStr)
      }
    }

    putOrder(om.buyOrder)
    putOrder(om.sellOrder)

    removeObsoleteDays(blockTs)
  }

  def removeObsoleteDays(timestamp: Long): Unit = {
    val ts = calcStartDay(timestamp)
    val daysToRemove = savedDays.keySet().filter(t => t < ts)
    if (daysToRemove.nonEmpty) {
      savedDays.synchronized {
        daysToRemove.filter(t => db.hasMap(OrderMatchTx + t)).foreach { d =>
          db.removeMap(orderMatchTxByDay(d))
        }
      }
    }
  }

  def calcStartDay(t: Long): Long = {
    val ts = t / 1000
    ts - ts % (24 * 60 * 60)
  }

  private val emptyTxIdSeq = Array.empty[String]

  private def parseTxSeq(a: Array[String]): Set[OrderMatch] = {
    a.toSet.flatMap { s: String => Base58.decode(s).toOption }.flatMap { id =>
      OrderMatch.parseBytes(transactionsMap.get(id)).toOption
    }
  }

  def findPrevOrderMatchTxs(om: OrderMatch): Set[OrderMatch] = {
    findPrevOrderMatchTxs(om.buyOrder) ++ findPrevOrderMatchTxs(om.sellOrder)
  }

  def findPrevOrderMatchTxs(order: Order): Set[OrderMatch] = {
    val orderDay = calcStartDay(order.maxTimestamp)
    if (savedDays.contains(orderDay)) {
      parseTxSeq(Option(orderMatchTxByDay(calcStartDay(order.maxTimestamp)).get(Base58.encode(order.id))).getOrElse(emptyTxIdSeq))
    } else Set.empty[OrderMatch]
  }

  def isOrderMatchValid(om: OrderMatch): Boolean = {
    def isNotCancelled(id: String) =
      Option(orderToCancelTx.get(id)).forall(!transactionsMap.containsKey(_))

    om.isValid(findPrevOrderMatchTxs(om)) && isNotCancelled(om.buyOrder.idStr) && isNotCancelled(om.sellOrder.idStr)
  }

  def putOrderCancel(oc: OrderCancelTransaction): Unit = {
    orderToCancelTx.put(oc.orderIdStr, oc.id)
  }

  def filterCancelOrderMatch(trans: Seq[Transaction]): Seq[Transaction] = {
    trans.foldLeft(Set.empty[String] -> Seq.empty[Transaction]) { case ((c, v), t) =>
      t match {
        case tt: OrderCancelTransaction => (c + tt.orderIdStr) -> (v :+ tt)
        case tt: OrderMatch if c(tt.buyOrder.idStr) || c(tt.sellOrder.idStr) => c -> v
        case _ => c -> (v :+ t)
      }
    }._2
  }
}
