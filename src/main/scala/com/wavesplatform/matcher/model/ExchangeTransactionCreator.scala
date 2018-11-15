package com.wavesplatform.matcher.model

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.model.Events.OrderExecuted
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.transaction.{AssetId, ValidationError}
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
    def assetFee(assetId: AssetId) = if (blockchain.hasAssetScript(assetId)) CommonValidation.ScriptExtraFee else 0L
    val accFee                     = if (blockchain.hasScript(o.sender)) CommonValidation.ScriptExtraFee else 0

    accFee + o.assetPair.amountAsset.fold(0L)(assetFee) + o.assetPair.priceAsset.fold(0L)(assetFee)
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
