package com.wavesplatform.state2.diffs

import cats._
import cats.implicits._
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.StateValidationError
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.assets.exchange.ExchangeTransaction

import scala.util.Right

object ExchangeTransactionDiff {

  def apply(s: StateReader, height: Int)(tx: ExchangeTransaction): Either[StateValidationError, Diff] = for {
    t <- enoughVolume(tx, s)
    buyPriceAssetChange <- t.buyOrder.getSpendAmount(t.price, t.amount).liftValidationError(tx).map(-_)
    buyAmountAssetChange <- t.buyOrder.getReceiveAmount(t.price, t.amount).liftValidationError(tx)
    sellPriceAssetChange <- t.sellOrder.getReceiveAmount(t.price, t.amount).liftValidationError(tx)
    sellAmountAssetChange <- t.sellOrder.getSpendAmount(t.price, t.amount).liftValidationError(tx).map(-_)
  } yield {
    val matcher = t.buyOrder.matcherPublicKey.toAccount
    val buyer = t.buyOrder.senderPublicKey.toAccount
    val seller = t.sellOrder.senderPublicKey.toAccount

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

    lazy val orderExchangeTxsMap: Map[ByteArray, Set[ExchangeTransaction]] = Map(
      EqByteArray(tx.buyOrder.id) -> Set(tx),
      EqByteArray(tx.sellOrder.id) -> Set(tx))

    Diff(height, tx, portfolios = portfolios, orderFills = Map(
      EqByteArray(tx.buyOrder.id) -> OrderFillInfo(tx.amount, tx.buyMatcherFee),
      EqByteArray(tx.sellOrder.id) -> OrderFillInfo(tx.amount, tx.sellMatcherFee)
    ))
  }


  private def enoughVolume(exTrans: ExchangeTransaction, s: StateReader): Either[StateValidationError, ExchangeTransaction] = {
    val filledBuy = s.filledVolumeAndFee(EqByteArray(exTrans.buyOrder.id))
    val filledSell = s.filledVolumeAndFee(EqByteArray(exTrans.sellOrder.id))

    val buyTotal = filledBuy.volume + exTrans.amount
    val sellTotal = filledSell.volume + exTrans.amount
    lazy val buyAmountValid = exTrans.buyOrder.amount >= buyTotal
    lazy val sellAmountValid = exTrans.sellOrder.amount >= sellTotal

    def isFeeValid(fee: Long, feeTotal: Long, amountTotal: Long, maxfee: Long, maxAmount: Long): Boolean =
      fee > 0 && feeTotal <= BigInt(maxfee) * BigInt(amountTotal) / BigInt(maxAmount)

    lazy val buyFeeValid = isFeeValid(fee = exTrans.buyMatcherFee,
      feeTotal = filledBuy.fee + exTrans.buyMatcherFee,
      amountTotal = buyTotal,
      maxfee = exTrans.buyOrder.matcherFee,
      maxAmount = exTrans.buyOrder.amount)

    lazy val sellFeeValid = isFeeValid(fee = exTrans.sellMatcherFee,
      feeTotal = filledSell.fee + exTrans.sellMatcherFee,
      amountTotal = sellTotal,
      maxfee = exTrans.sellOrder.matcherFee,
      maxAmount = exTrans.sellOrder.amount)

    if (!buyAmountValid) Left(TransactionValidationError(exTrans, s"Too much buy. Already filled volume for the order: ${filledBuy.volume}"))
    else if (!sellAmountValid) Left(TransactionValidationError(exTrans, s"Too much sell. Already filled volume for the order: ${filledSell.volume}"))
    else if (!buyFeeValid) Left(TransactionValidationError(exTrans, s"Insufficient buy fee"))
    else if (!sellFeeValid) Left(TransactionValidationError(exTrans, s"Insufficient sell fee"))
    else Right(exTrans)
  }
}
