package com.wavesplatform.state2.diffs

import cats._
import cats.implicits._
import com.wavesplatform.state2._
import com.wavesplatform.state2.reader.StateReader
import scorex.transaction.{StateValidationError, ValidationError}
import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.assets.exchange.ExchangeTransaction

import scala.util.Right

object ExchangeTransactionDiff {

  def apply(s: StateReader, height: Int)(tx: ExchangeTransaction): Either[StateValidationError, Diff] = for {
    t <- isValidAgainstPreviousTxs(tx, s.findPreviousExchangeTxs(tx.sellOrder) ++ s.findPreviousExchangeTxs(tx.buyOrder))
    buyPriceAssetChange <- t.buyOrder.getSpendAmount(t.price, t.amount).liftValidationError(tx).map(-_)
    buyAmountAssetChange <- t.buyOrder.getReceiveAmount(t.price, t.amount).liftValidationError(tx)
    sellPriceAssetChange <- t.sellOrder.getReceiveAmount(t.price, t.amount).liftValidationError(tx)
    sellAmountAssetChange <- t.sellOrder.getSpendAmount(t.price, t.amount).liftValidationError(tx).map(-_)
  } yield {
    val matcher = t.buyOrder.matcherPublicKey.toAccount
    val buyer = t.buyOrder.senderPublicKey.toAccount
    val seller = t.sellOrder.senderPublicKey.toAccount

    def wavesPortfolio(amt: Long) = Portfolio(amt, amt, Map.empty)

    val feeDiff = Monoid.combineAll(Seq(
      Map(matcher -> wavesPortfolio(t.buyMatcherFee + t.sellMatcherFee - t.fee)),
      Map(buyer -> wavesPortfolio(-t.buyMatcherFee)),
      Map(seller -> wavesPortfolio(-t.sellMatcherFee))))

    val priceDiff = t.buyOrder.assetPair.priceAsset.map(EqByteArray) match {
      case Some(assetId) => Monoid.combine(
        Map(buyer -> Portfolio(0, 0, Map(assetId -> buyPriceAssetChange))),
        Map(seller -> Portfolio(0, 0, Map(assetId -> sellPriceAssetChange))))
      case None => Monoid.combine(
        Map(buyer -> Portfolio(buyPriceAssetChange, buyPriceAssetChange, Map.empty)),
        Map(seller -> Portfolio(sellPriceAssetChange, sellPriceAssetChange, Map.empty)))
    }

    val amountDiff = t.buyOrder.assetPair.amountAsset.map(EqByteArray) match {
      case Some(assetId) => Monoid.combine(
        Map(buyer -> Portfolio(0, 0, Map(assetId -> buyAmountAssetChange))),
        Map(seller -> Portfolio(0, 0, Map(assetId -> sellAmountAssetChange))))
      case None => Monoid.combine(
        Map(buyer -> Portfolio(buyAmountAssetChange, buyAmountAssetChange, Map.empty)),
        Map(seller -> Portfolio(sellAmountAssetChange, sellAmountAssetChange, Map.empty)))
    }

    val result = Monoid.combineAll(Seq(feeDiff, priceDiff, amountDiff))

    Diff(height, tx, result)
  }


  private def isValidAgainstPreviousTxs(exTrans: ExchangeTransaction,
                                        previousMatches: Set[ExchangeTransaction]): Either[StateValidationError, ExchangeTransaction] = {

    lazy val buyTransactions = previousMatches.filter { om =>
      om.buyOrder.id sameElements exTrans.buyOrder.id
    }
    lazy val sellTransactions = previousMatches.filter { om =>
      om.sellOrder.id sameElements exTrans.sellOrder.id
    }

    lazy val buyTotal = buyTransactions.foldLeft(0L)(_ + _.amount) + exTrans.amount
    lazy val sellTotal = sellTransactions.foldLeft(0L)(_ + _.amount) + exTrans.amount

    lazy val buyFeeTotal = buyTransactions.map(_.buyMatcherFee).sum + exTrans.buyMatcherFee
    lazy val sellFeeTotal = sellTransactions.map(_.sellMatcherFee).sum + exTrans.sellMatcherFee

    lazy val amountIsValid: Boolean = {
      val b = buyTotal <= exTrans.buyOrder.amount
      val s = sellTotal <= exTrans.sellOrder.amount
      b && s
    }

    def isFeeValid(fee: Long, feeTotal: Long, amountTotal: Long, maxfee: Long, maxAmount: Long): Boolean = {
      fee > 0 &&
        feeTotal <= BigInt(maxfee) * BigInt(amountTotal) / BigInt(maxAmount)
    }

    if (!amountIsValid) {
      Left(TransactionValidationError(exTrans, "Insufficient amount to buy or sell"))
    } else if (!isFeeValid(exTrans.buyMatcherFee, buyFeeTotal, buyTotal, exTrans.buyOrder.matcherFee, exTrans.buyOrder.amount) ||
      !isFeeValid(exTrans.sellMatcherFee, sellFeeTotal, sellTotal, exTrans.sellOrder.matcherFee, exTrans.sellOrder.amount)) {
      Left(TransactionValidationError(exTrans, "Insufficient fee"))
    } else Right(exTrans)
  }
}
