package com.wavesplatform.state.diffs

import cats._
import cats.implicits._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state._
import com.wavesplatform.transaction.ValidationError
import com.wavesplatform.transaction.ValidationError.{GenericError, OrderValidationError}
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction

import scala.util.Right

object ExchangeTransactionDiff {

  def apply(blockchain: Blockchain, height: Int)(tx: ExchangeTransaction): Either[ValidationError, Diff] = {
    val matcher = tx.buyOrder.matcherPublicKey.toAddress
    val buyer   = tx.buyOrder.senderPublicKey.toAddress
    val seller  = tx.sellOrder.senderPublicKey.toAddress
    val assetIds = Set(tx.buyOrder.assetPair.amountAsset,
                       tx.buyOrder.assetPair.priceAsset,
                       tx.sellOrder.assetPair.amountAsset,
                       tx.sellOrder.assetPair.priceAsset).flatten
    val assets             = assetIds.map(blockchain.assetDescription)
    val smartTradesEnabled = blockchain.activatedFeatures.contains(BlockchainFeatures.SmartAccountTrading.id)
    val smartAssetsEnabled = blockchain.activatedFeatures.contains(BlockchainFeatures.SmartAssets.id)
    for {
      _ <- Either.cond(assets.forall(_.isDefined), (), GenericError("Assets should be issued before they can be traded"))
      _ <- Either.cond(
        smartAssetsEnabled || !assets.exists(_.flatMap(_.script).isDefined),
        (),
        GenericError(s"Smart assets can't participate in ExchangeTransactions (SmartAssetsFeature is disabled)")
      )
      _ <- Either.cond(
        smartTradesEnabled || !blockchain.hasScript(buyer),
        (),
        GenericError(s"Buyer $buyer can't participate in ExchangeTransaction because it has assigned Script (SmartAccountsTrades is disabled)")
      )
      _ <- Either.cond(
        smartTradesEnabled || !blockchain.hasScript(seller),
        (),
        GenericError(s"Seller $seller can't participate in ExchangeTransaction because it has assigned Script (SmartAccountsTrades is disabled)")
      )
      t                     <- enoughVolume(tx, blockchain)
      buyPriceAssetChange   <- t.buyOrder.getSpendAmount(t.amount, t.price).liftValidationError(tx).map(-_)
      buyAmountAssetChange  <- t.buyOrder.getReceiveAmount(t.amount, t.price).liftValidationError(tx)
      sellPriceAssetChange  <- t.sellOrder.getReceiveAmount(t.amount, t.price).liftValidationError(tx)
      sellAmountAssetChange <- t.sellOrder.getSpendAmount(t.amount, t.price).liftValidationError(tx).map(-_)
    } yield {

      def wavesPortfolio(amt: Long) = Portfolio(amt, LeaseBalance.empty, Map.empty)

      val feeDiff = Monoid.combineAll(
        Seq(
          Map(matcher -> wavesPortfolio(t.buyMatcherFee + t.sellMatcherFee - t.fee)),
          Map(buyer   -> wavesPortfolio(-t.buyMatcherFee)),
          Map(seller  -> wavesPortfolio(-t.sellMatcherFee))
        ))

      val priceDiff = t.buyOrder.assetPair.priceAsset match {
        case Some(assetId) =>
          Monoid.combine(
            Map(buyer  -> Portfolio(0, LeaseBalance.empty, Map(assetId -> buyPriceAssetChange))),
            Map(seller -> Portfolio(0, LeaseBalance.empty, Map(assetId -> sellPriceAssetChange)))
          )
        case None =>
          Monoid.combine(Map(buyer  -> Portfolio(buyPriceAssetChange, LeaseBalance.empty, Map.empty)),
                         Map(seller -> Portfolio(sellPriceAssetChange, LeaseBalance.empty, Map.empty)))
      }

      val amountDiff = t.buyOrder.assetPair.amountAsset match {
        case Some(assetId) =>
          Monoid.combine(
            Map(buyer  -> Portfolio(0, LeaseBalance.empty, Map(assetId -> buyAmountAssetChange))),
            Map(seller -> Portfolio(0, LeaseBalance.empty, Map(assetId -> sellAmountAssetChange)))
          )
        case None =>
          Monoid.combine(Map(buyer  -> Portfolio(buyAmountAssetChange, LeaseBalance.empty, Map.empty)),
                         Map(seller -> Portfolio(sellAmountAssetChange, LeaseBalance.empty, Map.empty)))
      }

      val portfolios = Monoid.combineAll(Seq(feeDiff, priceDiff, amountDiff))

      Diff(
        height,
        tx,
        portfolios = portfolios,
        orderFills = Map(
          tx.buyOrder.id()  -> VolumeAndFee(tx.amount, tx.buyMatcherFee),
          tx.sellOrder.id() -> VolumeAndFee(tx.amount, tx.sellMatcherFee)
        )
      )
    }
  }

  private def enoughVolume(exTrans: ExchangeTransaction, blockchain: Blockchain): Either[ValidationError, ExchangeTransaction] = {
    val filledBuy  = blockchain.filledVolumeAndFee(exTrans.buyOrder.id())
    val filledSell = blockchain.filledVolumeAndFee(exTrans.sellOrder.id())

    val buyTotal             = filledBuy.volume + exTrans.amount
    val sellTotal            = filledSell.volume + exTrans.amount
    lazy val buyAmountValid  = exTrans.buyOrder.amount >= buyTotal
    lazy val sellAmountValid = exTrans.sellOrder.amount >= sellTotal

    def isFeeValid(feeTotal: Long, amountTotal: Long, maxfee: Long, maxAmount: Long): Boolean =
      feeTotal <= BigInt(maxfee) * BigInt(amountTotal) / BigInt(maxAmount)

    lazy val buyFeeValid = isFeeValid(feeTotal = filledBuy.fee + exTrans.buyMatcherFee,
                                      amountTotal = buyTotal,
                                      maxfee = exTrans.buyOrder.matcherFee,
                                      maxAmount = exTrans.buyOrder.amount)

    lazy val sellFeeValid = isFeeValid(feeTotal = filledSell.fee + exTrans.sellMatcherFee,
                                       amountTotal = sellTotal,
                                       maxfee = exTrans.sellOrder.matcherFee,
                                       maxAmount = exTrans.sellOrder.amount)

    if (!buyAmountValid) Left(OrderValidationError(exTrans.buyOrder, s"Too much buy. Already filled volume for the order: ${filledBuy.volume}"))
    else if (!sellAmountValid)
      Left(OrderValidationError(exTrans.sellOrder, s"Too much sell. Already filled volume for the order: ${filledSell.volume}"))
    else if (!buyFeeValid) Left(OrderValidationError(exTrans.buyOrder, s"Insufficient buy fee"))
    else if (!sellFeeValid) Left(OrderValidationError(exTrans.sellOrder, s"Insufficient sell fee"))
    else Right(exTrans)
  }
}
