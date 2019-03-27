package com.wavesplatform.matcher.model

import com.wavesplatform.account.{Address, PrivateKeyAccount}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider.FeatureProviderExt
import com.wavesplatform.matcher.model.ExchangeTransactionCreator._
import com.wavesplatform.settings.AssetType
import com.wavesplatform.settings.AssetType.AssetType
import com.wavesplatform.settings.OrderFeeSettings.{OrderFeeSettings, PercentSettings}
import com.wavesplatform.state.Blockchain
import com.wavesplatform.state.diffs.CommonValidation
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange._
import com.wavesplatform.transaction.{Asset, ValidationError}

class ExchangeTransactionCreator(blockchain: Blockchain, matcherPrivateKey: PrivateKeyAccount, matcherOrderFeeSettings: OrderFeeSettings) {

  private def calculateMatcherFee(buy: Order, sell: Order, executedAmount: Long, executedPrice: Long): (Long, Long) = {

    def calcFee(o: Order, txAmount: Long, totalAmount: Long): Long = {
      val p = BigInt(txAmount) * o.matcherFee / totalAmount
      p.toLong
    }

    def getBuySellAmountTxOrTotal(assetType: AssetType, buyAmount: Long, buyPrice: Long, sellAmount: Long, sellPrice: Long): (Long, Long) = {
      val (buyAmt, sellAmt) = assetType match {
        case AssetType.AMOUNT    => buy.getReceiveAmount(buyAmount, buyPrice) -> sell.getSpendAmount(sellAmount, sellPrice)
        case AssetType.PRICE     => buy.getSpendAmount(buyAmount, buyPrice)   -> sell.getReceiveAmount(sellAmount, sellPrice)
        case AssetType.RECEIVING => buy.getReceiveAmount(buyAmount, buyPrice) -> sell.getReceiveAmount(sellAmount, sellPrice)
        case AssetType.SPENDING  => buy.getSpendAmount(buyAmount, buyPrice)   -> sell.getSpendAmount(sellAmount, sellPrice)
      }

      buyAmt.explicitGet() -> sellAmt.explicitGet()
    }

    matcherOrderFeeSettings match {
      case PercentSettings(assetType, _) =>
        val (buyAmountFromTx, sellAmountFromTx) = getBuySellAmountTxOrTotal(assetType, executedAmount, executedPrice, executedAmount, executedPrice)
        val (buyAmountTotal, sellAmountTotal)   = getBuySellAmountTxOrTotal(assetType, buy.amount, buy.price, sell.amount, sell.price)

        (
          Math.min(buy.matcherFee, calcFee(buy, buyAmountFromTx, buyAmountTotal)),
          Math.min(sell.matcherFee, calcFee(sell, sellAmountFromTx, sellAmountTotal))
        )

      case _ => calcFee(buy, executedAmount, buy.amount) -> calcFee(sell, executedAmount, sell.amount)
    }
  }

  def createTransaction(submitted: LimitOrder, counter: LimitOrder, timestamp: Long): Either[ValidationError, ExchangeTransaction] = {
    val executedAmount    = LimitOrder.executedAmount(submitted, counter)
    val price             = counter.price
    val (buy, sell)       = Order.splitByType(submitted.order, counter.order)
    val (buyFee, sellFee) = calculateMatcherFee(buy, sell, executedAmount, price)

    val txFee = minFee(blockchain, matcherPrivateKey, counter.order.assetPair, OrderValidator.exchangeTransactionCreationFee)
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
    case _          => Left(ValidationError.ActivationError("Smart Account Trading feature has not been activated yet"))
  }
}

object ExchangeTransactionCreator {

  type CreateTransaction = (LimitOrder, LimitOrder, Long) => Either[ValidationError, ExchangeTransaction]

  /**
    * @see [[com.wavesplatform.transaction.smart.Verifier#verifyExchange verifyExchange]]
    */
  def minFee(blockchain: Blockchain, matcherAddress: Address, assetPair: AssetPair, baseFee: Long): Long = {

    def assetFee(assetId: Asset): Long = assetId match {
      case Waves => 0L
      case asset: IssuedAsset =>
        if (blockchain.hasAssetScript(asset)) CommonValidation.ScriptExtraFee
        else 0L
    }

    baseFee +
      minAccountFee(blockchain, matcherAddress) +
      assetPair.amountAsset.fold(0L)(assetFee) +
      assetPair.priceAsset.fold(0L)(assetFee)
  }

  def minAccountFee(blockchain: Blockchain, address: Address): Long = {
    if (blockchain hasScript address) CommonValidation.ScriptExtraFee else 0L
  }
}
