package com.wavesplatform.matcher.model

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.model.Events.OrderExecuted
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.utils.Time

class ExchangeTransactionCreator(blockchain: Blockchain, matcherPrivateKey: PrivateKeyAccount, settings: MatcherSettings, time: Time) {
  private def calculateMatcherFee(buy: Order, sell: Order, amount: Long): (Long, Long) = {
    def calcFee(o: Order, amount: Long): Long = {
      val p = BigInt(amount) * o.matcherFee / o.amount
      p.toLong
    }

    (calcFee(buy, amount), calcFee(sell, amount))
  }

  def calcExtraTxFee(o: Order): Long = {
    if (blockchain.hasScript(o.sender)) CommonValidation.ScriptExtraFee
    else 0
  }

  def createTransaction(event: OrderExecuted): Either[ValidationError, ExchangeTransaction] = {
    import event.{counter, submitted}
    val price             = counter.price
    val (buy, sell)       = Order.splitByType(submitted.order, counter.order)
    val (buyFee, sellFee) = calculateMatcherFee(buy, sell, event.executedAmount)
    val txFee             = settings.orderMatchTxFee + calcExtraTxFee(buy) + calcExtraTxFee(sell)
    ExchangeTransactionV2.create(matcherPrivateKey, buy, sell, event.executedAmount, price, buyFee, sellFee, txFee, time.getTimestamp())
  }
}
