package com.wavesplatform.state2.diffs

import cats._
import cats.implicits._
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.{SnapshotStateReader}
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.OrderValidationError
import scorex.transaction.assets.exchange.ExchangeTransaction

import scala.util.Right

object ExchangeTransactionDiff {

  def apply(s: SnapshotStateReader, height: Int)(tx: ExchangeTransaction): Either[ValidationError, Diff] = for {
    t <- enoughVolume(tx, s)
    buyPriceAssetChange <- t.buyOrder.getSpendAmount(t.price, t.amount).liftValidationError(tx).map(-_)
    buyAmountAssetChange <- t.buyOrder.getReceiveAmount(t.price, t.amount).liftValidationError(tx)
    sellPriceAssetChange <- t.sellOrder.getReceiveAmount(t.price, t.amount).liftValidationError(tx)
    sellAmountAssetChange <- t.sellOrder.getSpendAmount(t.price, t.amount).liftValidationError(tx).map(-_)
  } yield {
    val matcher = t.buyOrder.matcherPublicKey.toAddress
    val buyer = t.buyOrder.senderPublicKey.toAddress
    val seller = t.sellOrder.senderPublicKey.toAddress

    def wavesPortfolio(amt: Long) = Portfolio(amt, LeaseInfo.empty, Map.empty)

    val feeDiff = Monoid.combineAll(Seq(
      Map(matcher -> wavesPortfolio(t.buyMatcherFee + t.sellMatcherFee - t.fee)),
      Map(buyer -> wavesPortfolio(-t.buyMatcherFee)),
      Map(seller -> wavesPortfolio(-t.sellMatcherFee))))

    val priceDiff = t.buyOrder.assetPair.priceAsset match {
      case Some(assetId) => Monoid.combine(
        Map(buyer -> Portfolio(0, LeaseInfo.empty, Map(assetId -> buyPriceAssetChange))),
        Map(seller -> Portfolio(0, LeaseInfo.empty, Map(assetId -> sellPriceAssetChange))))
      case None => Monoid.combine(
        Map(buyer -> Portfolio(buyPriceAssetChange, LeaseInfo.empty, Map.empty)),
        Map(seller -> Portfolio(sellPriceAssetChange, LeaseInfo.empty, Map.empty)))
    }

    val amountDiff = t.buyOrder.assetPair.amountAsset match {
      case Some(assetId) => Monoid.combine(
        Map(buyer -> Portfolio(0, LeaseInfo.empty, Map(assetId -> buyAmountAssetChange))),
        Map(seller -> Portfolio(0, LeaseInfo.empty, Map(assetId -> sellAmountAssetChange))))
      case None => Monoid.combine(
        Map(buyer -> Portfolio(buyAmountAssetChange, LeaseInfo.empty, Map.empty)),
        Map(seller -> Portfolio(sellAmountAssetChange, LeaseInfo.empty, Map.empty)))
    }

    val portfolios = Monoid.combineAll(Seq(feeDiff, priceDiff, amountDiff))

    Diff(height, tx, portfolios = portfolios, orderFills = Map(
      ByteStr(tx.buyOrder.id()) -> OrderFillInfo(tx.amount, tx.buyMatcherFee),
      ByteStr(tx.sellOrder.id()) -> OrderFillInfo(tx.amount, tx.sellMatcherFee)
    ))
  }


  private def enoughVolume(exTrans: ExchangeTransaction, s: SnapshotStateReader): Either[ValidationError, ExchangeTransaction] = {
    val filledBuy = s.filledVolumeAndFee(ByteStr(exTrans.buyOrder.id()))
    val filledSell = s.filledVolumeAndFee(ByteStr(exTrans.sellOrder.id()))

    val buyTotal = filledBuy.volume + exTrans.amount
    val sellTotal = filledSell.volume + exTrans.amount
    lazy val buyAmountValid = exTrans.buyOrder.amount >= buyTotal
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
    else if (!sellAmountValid) Left(OrderValidationError(exTrans.sellOrder, s"Too much sell. Already filled volume for the order: ${filledSell.volume}"))
    else if (!buyFeeValid) Left(OrderValidationError(exTrans.buyOrder, s"Insufficient buy fee"))
    else if (!sellFeeValid) Left(OrderValidationError(exTrans.sellOrder, s"Insufficient sell fee"))
    else Right(exTrans)
  }
}
