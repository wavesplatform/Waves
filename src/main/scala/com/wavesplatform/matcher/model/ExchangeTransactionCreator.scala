package com.wavesplatform.matcher.model

import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.model.Events.OrderExecuted
import com.wavesplatform.matcher.model.ExchangeTransactionCreator._
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.{AssetId, ValidationError}
import com.wavesplatform.utils.Time

class ExchangeTransactionCreator(blockchain: Blockchain, matcherPrivateKey: PrivateKeyAccount, settings: MatcherSettings, time: Time) {
  private def calculateMatcherFee(buy: Order, sell: Order, amount: Long): (Long, Long) = {
    def calcFee(o: Order, amount: Long): Long = {
      val p = BigInt(amount) * o.matcherFee / o.amount
      p.toLong
    }

    (calcFee(buy, amount), calcFee(sell, amount))
  }

  def createTransaction(event: OrderExecuted): Either[ValidationError, ExchangeTransaction] = {
    import event.{counter, submitted}
    val price             = counter.price
    val (buy, sell)       = Order.splitByType(submitted.order, counter.order)
    val (buyFee, sellFee) = calculateMatcherFee(buy, sell, event.executedAmount)

    val txFee = getMinFee(blockchain, settings.orderMatchTxFee, matcherPrivateKey, Some(buy.sender), Some(sell.sender), counter.order.assetPair)
    ExchangeTransactionV2.create(matcherPrivateKey, buy, sell, event.executedAmount, price, buyFee, sellFee, txFee, time.getTimestamp())
  }
}

object ExchangeTransactionCreator {

  /**
    * @note see Verifier.verifyExchange
    */
  def getMinFee(blockchain: Blockchain,
                orderMatchTxFee: Long,
                matcherAddress: Address,
                order1Sender: Option[Address],
                order2Sender: Option[Address],
                assetPair: AssetPair): Long = {
    def assetFee(assetId: AssetId): Long   = if (blockchain.hasAssetScript(assetId)) CommonValidation.ScriptExtraFee else 0L
    def accountFee(address: Address): Long = if (blockchain.hasScript(address)) CommonValidation.ScriptExtraFee else 0L

    orderMatchTxFee +
      accountFee(matcherAddress) +
      assetPair.amountAsset.fold(0L)(assetFee) +
      assetPair.priceAsset.fold(0L)(assetFee)
  }

}
