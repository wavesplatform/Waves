package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.model.Events.OrderExecuted
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.utx.UtxPool
import scorex.transaction.ValidationError
import scorex.transaction.assets.exchange.{ExchangeTransaction, Order}
import scorex.utils.{NTP, ScorexLogging}
import scorex.wallet.Wallet

trait ExchangeTransactionCreator extends ScorexLogging {
  val functionalitySettings: FunctionalitySettings
  val blockchain: Blockchain
  val wallet: Wallet
  val settings: MatcherSettings
  val utx: UtxPool
  private var txTime: Long = 0

  private def getTimestamp: Long = {
    txTime = Math.max(NTP.correctedTime(), txTime + 1)
    txTime
  }

  def createTransaction(event: OrderExecuted): Either[ValidationError, ExchangeTransaction] = {
    import event.{counter, submitted}
    wallet
      .privateKeyAccount(submitted.order.matcherPublicKey)
      .flatMap(matcherPrivateKey => {
        val price             = counter.price
        val (buy, sell)       = Order.splitByType(submitted.order, counter.order)
        val (buyFee, sellFee) = calculateMatcherFee(buy, sell, event.executedAmount)
        ExchangeTransaction.create(matcherPrivateKey, buy, sell, price, event.executedAmount, buyFee, sellFee, settings.orderMatchTxFee, getTimestamp)
      })
  }

  def calculateMatcherFee(buy: Order, sell: Order, amount: Long): (Long, Long) = {
    def calcFee(o: Order, amount: Long): Long = {
      val p = BigInt(amount) * o.matcherFee / o.amount
      p.toLong
    }

    (calcFee(buy, amount), calcFee(sell, amount))
  }
}
