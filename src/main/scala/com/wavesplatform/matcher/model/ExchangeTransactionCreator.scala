package com.wavesplatform.matcher.model

import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider.FeatureProviderExt
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.model.ExchangeTransactionCreator._
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.{AssetId, ValidationError}

class ExchangeTransactionCreator(blockchain: Blockchain, matcherPrivateKey: PrivateKeyAccount, settings: MatcherSettings) {
  private def calculateMatcherFee(buy: Order, sell: Order, amount: Long): (Long, Long) = {
    def calcFee(o: Order, amount: Long): Long = {
      val p = BigInt(amount) * o.matcherFee / o.amount
      p.toLong
    }

    (calcFee(buy, amount), calcFee(sell, amount))
  }

  def createTransaction(submitted: LimitOrder, counter: LimitOrder, timestamp: Long): Either[ValidationError, ExchangeTransaction] = {
    val executedAmount    = LimitOrder.executedAmount(submitted, counter)
    val price             = counter.price
    val (buy, sell)       = Order.splitByType(submitted.order, counter.order)
    val (buyFee, sellFee) = calculateMatcherFee(buy, sell, executedAmount)

    val txFee = minFee(blockchain, settings.orderMatchTxFee, matcherPrivateKey, counter.order.assetPair)
    if (blockchain.isFeatureActivated(BlockchainFeatures.SmartAccountTrading, blockchain.height))
      ExchangeTransactionV2.create(matcherPrivateKey, buy, sell, executedAmount, price, buyFee, sellFee, txFee, timestamp)
    else
      for {
        buyV1  <- toV1(buy)
        sellV1 <- toV1(sell)
        tx     <- ExchangeTransactionV1.create(matcherPrivateKey, buyV1, sellV1, executedAmount, price, buyFee, sellFee, txFee, timestamp)
      } yield tx
  }

  private def toV1(order: Order): Either[ValidationError, OrderV1] = order match {
    case x: OrderV1 => Right(x)
    case _          => Left(ValidationError.ActivationError("SmartAccountTrading has not been activated yet"))
  }
}

object ExchangeTransactionCreator {

  type CreateTransaction = (LimitOrder, LimitOrder, Long) => Either[ValidationError, ExchangeTransaction]

  /**
    * @see [[com.wavesplatform.transaction.smart.Verifier#verifyExchange verifyExchange]]
    */
  def minFee(blockchain: Blockchain, orderMatchTxFee: Long, matcherAddress: Address, assetPair: AssetPair): Long = {
    def assetFee(assetId: AssetId): Long   = if (blockchain.hasAssetScript(assetId)) CommonValidation.ScriptExtraFee else 0L
    def accountFee(address: Address): Long = if (blockchain.hasScript(address)) CommonValidation.ScriptExtraFee else 0L

    orderMatchTxFee +
      accountFee(matcherAddress) +
      assetPair.amountAsset.fold(0L)(assetFee) +
      assetPair.priceAsset.fold(0L)(assetFee)
  }
}
