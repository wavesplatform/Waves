package scorex.transaction.state.database.state.extension

import scorex.crypto.encode.Base58
import scorex.transaction.Transaction
import scorex.transaction.assets.exchange.{Order, OrderMatch}
import scorex.transaction.state.database.state.storage.{OrderMatchStorageI, StateStorageI}

class OrderMatchStoredState(storage: StateStorageI with OrderMatchStorageI) extends StateExtension {


  override def isValid(tx: Transaction): Boolean = tx match {
    case om: OrderMatch => isOrderMatchValid(om)
    case _ => true
  }

  override def process(tx: Transaction, blockTs: Long, height: Int): Unit = tx match {
    case om: OrderMatch => putOrderMatch(om, blockTs)
    case _ =>
  }

  val MaxLiveDays = (Order.MaxLiveTime / 24L * 60L * 60L * 1000L).toInt

  private def isOrderMatchValid(om: OrderMatch): Boolean = {
    om.isValid(findPrevOrderMatchTxs(om))
  }

  private def putOrderMatch(om: OrderMatch, blockTs: Long): Unit = {
    def isSaveNeeded(order: Order): Boolean = {
      order.maxTimestamp >= blockTs
    }

    def putOrder(order: Order) = {
      if (isSaveNeeded(order)) {
        val orderDay = calcStartDay(order.maxTimestamp)
        storage.putSavedDays(orderDay)
        val orderIdStr = Base58.encode(order.id)
        val omIdStr = Base58.encode(om.id)
        val prev = storage.getOrderMatchTxByDay(orderDay, orderIdStr).getOrElse(Array.empty[String])
        if (!prev.contains(omIdStr)) storage.putOrderMatchTxByDay(orderDay, orderIdStr, prev :+ omIdStr)
      }
    }

    putOrder(om.buyOrder)
    putOrder(om.sellOrder)

    removeObsoleteDays(blockTs)
  }

  private def calcStartDay(t: Long): Long = {
    val ts = t / 1000
    ts - ts % (24 * 60 * 60)
  }

  private def removeObsoleteDays(timestamp: Long): Unit = {
    val ts = calcStartDay(timestamp)
    val daysToRemove: List[Long] = storage.savedDaysKeys.filter(t => t < ts)
    if (daysToRemove.nonEmpty) {
      synchronized {
        storage.removeOrderMatchDays(daysToRemove)
      }
    }
  }

  private val emptyTxIdSeq = Array.empty[String]

  private def parseTxSeq(a: Array[String]): Set[OrderMatch] = {
    a.toSet.flatMap { s: String => Base58.decode(s).toOption }.flatMap { id =>
      storage.getTransactionBytes(id).flatMap(b => OrderMatch.parseBytes(b).toOption)
    }
  }

  private def findPrevOrderMatchTxs(om: OrderMatch): Set[OrderMatch] = {
    findPrevOrderMatchTxs(om.buyOrder) ++ findPrevOrderMatchTxs(om.sellOrder)
  }

  def findPrevOrderMatchTxs(order: Order): Set[OrderMatch] = {
    val orderDay = calcStartDay(order.maxTimestamp)
    if (storage.containsSavedDays(orderDay)) {
      parseTxSeq(storage.getOrderMatchTxByDay(calcStartDay(order.maxTimestamp), Base58.encode(order.id))
        .getOrElse(emptyTxIdSeq))
    } else Set.empty[OrderMatch]
  }
}
