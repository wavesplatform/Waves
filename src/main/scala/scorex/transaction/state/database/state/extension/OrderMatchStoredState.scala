package scorex.transaction.state.database.state.extension

import scorex.crypto.encode.Base58
import scorex.transaction.Transaction
import scorex.transaction.ValidationError.StateValidationError
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.database.state.storage.{OrderMatchStorageI, StateStorageI}

class OrderMatchStoredState(storage: StateStorageI with OrderMatchStorageI) extends Validator {

  override def validate(storedState: StoredState, tx: Transaction, height: Int): Either[StateValidationError, Transaction] = tx match {
    case om: ExchangeTransaction => OrderMatchStoredState.isOrderMatchValid(om, findPrevOrderMatchTxs(om))
    case _ => Right(tx)
  }

  override def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = tx match {
    case om: ExchangeTransaction => putOrderMatch(om, blockTs)
    case _ =>
  }

  private def putOrderMatch(om: ExchangeTransaction, blockTs: Long): Unit = {
    def isSaveNeeded(order: Order): Boolean = {
      order.expiration >= blockTs
    }

    def putOrder(order: Order) = {
      if (isSaveNeeded(order)) {
        val orderDay = calcStartDay(order.expiration)
        storage.putSavedDays(orderDay)
        val orderIdStr = Base58.encode(order.id)
        val omIdStr = Base58.encode(om.id)
        val prev = storage.getOrderMatchTxByDay(orderDay, orderIdStr).getOrElse(Array.empty[String])
        if (!prev.contains(omIdStr)) {
          storage.putOrderMatchTxByDay(orderDay, orderIdStr, prev :+ omIdStr)
        }
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

  private def parseTxSeq(a: Array[String]): Set[ExchangeTransaction] = {
    a.toSet.flatMap { s: String => Base58.decode(s).toOption }.flatMap { id =>
      storage.getTransactionBytes(id).flatMap(b => ExchangeTransaction.parseBytes(b).toOption)
    }
  }

  private def findPrevOrderMatchTxs(om: ExchangeTransaction): Set[ExchangeTransaction] = {
    findPrevOrderMatchTxs(om.buyOrder) ++ findPrevOrderMatchTxs(om.sellOrder)
  }

  def findPrevOrderMatchTxs(order: Order): Set[ExchangeTransaction] = {
    val orderDay = calcStartDay(order.expiration)
    if (storage.containsSavedDays(orderDay)) {
      parseTxSeq(storage.getOrderMatchTxByDay(calcStartDay(order.expiration), Base58.encode(order.id))
        .getOrElse(emptyTxIdSeq))
    } else Set.empty[ExchangeTransaction]
  }

  override def validateWithBlockTxs(storedState: StoredState, tx: Transaction,
                                    blockTxs: Seq[Transaction], height: Int): Either[StateValidationError, Transaction] = tx match {
    case om: ExchangeTransaction =>
      val thisExchanges: Set[ExchangeTransaction] = blockTxs.collect {
        case a: ExchangeTransaction if a != tx && (a.buyOrder == om.buyOrder || a.sellOrder == om.sellOrder) => a
      }.toSet

      OrderMatchStoredState.isOrderMatchValid(om, findPrevOrderMatchTxs(om) ++ thisExchanges)
    case _ => Right(tx)
  }
}


object OrderMatchStoredState {
  def isOrderMatchValid(exTrans: ExchangeTransaction, previousMatches: Set[ExchangeTransaction]): Either[StateValidationError, ExchangeTransaction] = {

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

    if (!amountIsValid) {
      Left(StateValidationError("Insufficient amount to buy or sell"))
    } else if (!isFeeValid(exTrans.buyMatcherFee, buyFeeTotal, buyTotal, exTrans.buyOrder.matcherFee, exTrans.buyOrder.amount) ||
      !isFeeValid(exTrans.sellMatcherFee, sellFeeTotal, sellTotal, exTrans.sellOrder.matcherFee, exTrans.sellOrder.amount)) {
      Left(StateValidationError("Insufficient fee"))
    } else Right(exTrans)
  }
}
