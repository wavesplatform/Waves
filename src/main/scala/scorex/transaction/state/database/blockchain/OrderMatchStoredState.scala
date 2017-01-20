package scorex.transaction.state.database.blockchain

import org.h2.mvstore.{MVMap, MVStore}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}

import scala.collection.JavaConversions._

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

  def putOrderMatch(om: ExchangeTransaction, blockTs: Long): Unit = {
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

  private def parseTxSeq(a: Array[String]): Set[ExchangeTransaction] = {
    a.toSet.flatMap { s: String => Base58.decode(s).toOption }.flatMap { id =>
      ExchangeTransaction.parseBytes(transactionsMap.get(id)).toOption
    }
  }

  def findPrevOrderMatchTxs(om: ExchangeTransaction): Set[ExchangeTransaction] = {
    findPrevOrderMatchTxs(om.buyOrder) ++ findPrevOrderMatchTxs(om.sellOrder)
  }

  def findPrevOrderMatchTxs(order: Order): Set[ExchangeTransaction] = {
    val orderDay = calcStartDay(order.maxTimestamp)
    if (savedDays.contains(orderDay)) {
      parseTxSeq(Option(orderMatchTxByDay(calcStartDay(order.maxTimestamp)).get(Base58.encode(order.id))).getOrElse(emptyTxIdSeq))
    } else Set.empty[ExchangeTransaction]
  }

  def isOrderMatchValid(exTrans: ExchangeTransaction): Boolean = {
    val previousMatches = findPrevOrderMatchTxs(exTrans)

    lazy val buyTransactions = previousMatches.filter { om =>
      om.buyOrder.id sameElements exTrans.buyOrder.id
    }
    lazy val sellTransactions = previousMatches.filter { om =>
      om.sellOrder.id sameElements exTrans.sellOrder.id
    }

    lazy val buyTotal = buyTransactions.foldLeft(0L)(_ + _.amount) + exTrans.amount
    lazy val sellTotal = sellTransactions.foldLeft(0L)(_ + _.amount) + exTrans.amount

    lazy val buyFeeTotal = buyTransactions.map(_.buyMatcherFee).sum + exTrans.buyMatcherFee
    lazy val sellFeeTotal = sellTransactions.map(_.sellMatcherFee).sum + exTrans.sellMatcherFee

    lazy val amountIsValid: Boolean = {
      val b = buyTotal <= exTrans.buyOrder.amount
      val s = sellTotal <= exTrans.sellOrder.amount
      b && s
    }

    def isFeeValid(fee: Long, feeTotal: Long, amountTotal: Long, maxfee: Long, maxAmount: Long): Boolean = {
      fee > 0 &&
        feeTotal <= BigInt(maxfee) * BigInt(amountTotal) / BigInt(maxAmount)
    }

    amountIsValid &&
      isFeeValid(exTrans.buyMatcherFee, buyFeeTotal, buyTotal, exTrans.buyOrder.matcherFee, exTrans.buyOrder.amount) &&
      isFeeValid(exTrans.sellMatcherFee, sellFeeTotal, sellTotal, exTrans.sellOrder.matcherFee, exTrans.sellOrder.amount)
  }
}
