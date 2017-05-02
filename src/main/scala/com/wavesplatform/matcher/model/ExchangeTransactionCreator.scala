package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.transaction.{SignedTransaction, TransactionModule, ValidationError}
import scorex.utils.{NTP, ScorexLogging, Time}
import scorex.wallet.Wallet

trait ExchangeTransactionCreator extends ScorexLogging {
  val transactionModule: TransactionModule
  val storedState: StateReader
  val wallet: Wallet
  val settings: MatcherSettings
  private var txTime: Long = 0

  private def getTimestamp: Long = {
    txTime = Math.max(NTP.correctedTime(), txTime + 1)
    txTime
  }

  def createTransaction(submitted: LimitOrder, counter: LimitOrder): Either[ValidationError, ExchangeTransaction] = {
    wallet.privateKeyAccount(submitted.order.matcherPublicKey).flatMap(matcherPrivateKey => {
      val price = counter.price
      val amount = math.min(submitted.amount, counter.amount)
      val (buy, sell) = Order.splitByType(submitted.order, counter.order)
      val (buyFee, sellFee) = calculateMatcherFee(buy, sell, amount: Long)
      ExchangeTransaction.create(matcherPrivateKey, buy, sell, price, amount, buyFee, sellFee, settings.orderMatchTxFee, getTimestamp)
    })
  }

  def calculateMatcherFee(buy: Order, sell: Order, amount: Long): (Long, Long) = {
    def calcFee(o: Order, amount: Long): Long = {
      val p = BigInt(amount) * o.matcherFee / o.amount
      p.toLong
    }
    (calcFee(buy, amount), calcFee(sell, amount))
  }

  def validate(orderMatch: ExchangeTransaction): Either[ValidationError, SignedTransaction] =
    transactionModule.validate(orderMatch)

  def sendToNetwork(tx: SignedTransaction): Either[ValidationError, SignedTransaction] = {
    transactionModule.onNewOffchainTransaction(tx)
  }
}
