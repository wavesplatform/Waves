package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.MatcherSettings
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.database.state.extension.OrderMatchStoredState
import scorex.transaction.{SignedTransaction, TransactionModule, ValidationError}
import scorex.utils.NTP
import scorex.wallet.Wallet

trait ExchangeTransactionCreator {
  val transactionModule: TransactionModule
  val storedState: StoredState
  val wallet: Wallet
  val settings: MatcherSettings
  //TODO ???
  val omss = storedState.validators.filter(_.isInstanceOf[OrderMatchStoredState]).head
    .asInstanceOf[OrderMatchStoredState]

  private var txTime: Long = 0

  private def getTimestamp: Long = {
    txTime = Math.max(NTP.correctedTime(), txTime + 1)
    txTime
  }

  def createTransaction(submitted: LimitOrder, counter: LimitOrder): Either[ValidationError, ExchangeTransaction] = {
    val matcher = wallet.privateKeyAccount(submitted.order.matcherPublicKey.address).get

    val price = counter.price
    val amount = math.min(submitted.amount, counter.amount)
    val (buy, sell) = Order.splitByType(submitted.order, counter.order)
    val (buyFee, sellFee) = calculateMatcherFee(buy, sell, amount: Long)
    ExchangeTransaction.create(matcher, buy, sell, price, amount, buyFee, sellFee, settings.orderMatchTxFee, getTimestamp)
  }

  def calculateMatcherFee(buy: Order, sell: Order, amount: Long): (Long, Long) = {
    def calcFee(o: Order, amount: Long): Long = {
      omss.findPrevOrderMatchTxs(o)
      val p = BigInt(amount) * o.matcherFee / o.amount
      p.toLong
    }

    (calcFee(buy, amount), calcFee(sell, amount))
  }

  def isValid(orderMatch: ExchangeTransaction): Boolean = {
    transactionModule.isValid(orderMatch, orderMatch.timestamp)
  }

  def sendToNetwork(tx: SignedTransaction): Unit = {
    transactionModule.onNewOffchainTransaction(tx)
  }
}
