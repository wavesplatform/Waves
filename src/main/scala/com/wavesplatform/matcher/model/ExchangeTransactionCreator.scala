package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.model.Events.OrderExecuted
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.Blockchain
import com.wavesplatform.utils.{NTP, ScorexLogging}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.wallet.Wallet

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
        (buy, sell) match {
          case (buy: OrderV1, sell: OrderV1) =>
            ExchangeTransactionV1
              .create(matcherPrivateKey, buy, sell, price, event.executedAmount, buyFee, sellFee, settings.orderMatchTxFee, getTimestamp)
          case _ =>
            ExchangeTransactionV2
              .create(matcherPrivateKey, buy, sell, price, event.executedAmount, buyFee, sellFee, settings.orderMatchTxFee, getTimestamp)
        }
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
