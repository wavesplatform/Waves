package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.market.OrderBookActor.CancelOrder
import com.wavesplatform.state2.reader.StateReader
import scorex.account.PublicKeyAccount
import scorex.transaction.ValidationError.CustomError
import scorex.transaction.assets.exchange.Validation.booleanOperators
import scorex.transaction.assets.exchange.{Order, Validation}
import scorex.transaction.{AssetAcc, AssetId}
import scorex.utils.NTP
import scorex.wallet.Wallet

trait OrderValidator {
  val orderHistory: OrderHistory
  val storedState: StateReader
  val settings: MatcherSettings
  val wallet: Wallet

  lazy val matcherPubKey: PublicKeyAccount = wallet.findWallet(settings.account).right.get

  def isBalanceWithOpenOrdersEnough(order: Order): Validation = {
    def notEnoughError(tBal: Long, oBal: Long, needs: Long): String = s"Not enough balance: [$tBal, $oBal], needs: $needs"

    val (acc, feeAcc) = (AssetAcc(order.senderPublicKey, order.getSpendAssetId), AssetAcc(order.senderPublicKey, None))

    val (assTBal, assOBal) = (storedState.spendableBalance(acc), orderHistory.getOpenVolume(acc))
    val (feeTBal, feeOBal) = (storedState.spendableBalance(feeAcc), orderHistory.getOpenVolume(feeAcc))

    if (acc != feeAcc) {
      (assTBal - assOBal >= order.getSpendAmount(order.price, order.amount).getOrElse(0L)) :|
        notEnoughError(assTBal, assOBal, order.getSpendAmount(order.price, order.amount).getOrElse(0L)) &&
        (feeTBal - feeOBal >= order.matcherFee) :| notEnoughError(feeTBal, feeOBal, order.matcherFee)
    }
    else {
      (assTBal - assOBal >= order.getSpendAmount(order.price, order.amount).getOrElse(0L) + order.matcherFee) :|
        notEnoughError(assTBal, assOBal, order.getSpendAmount(order.price, order.amount).getOrElse(0L) + order.matcherFee)
    }
  }

  def validateIntegerAmount(lo: LimitOrder): Validation = {
    def getDecimals(assetId: Option[AssetId]) = assetId.flatMap(storedState.getIssueTransaction).map(_.decimals.toInt).getOrElse(8)

    val amountDecimals = getDecimals(lo.order.assetPair.amountAsset)
    val priceDecimals = getDecimals(lo.order.assetPair.priceAsset)
    val price = BigDecimal(lo.price)*math.pow(10, amountDecimals - priceDecimals)/Order.PriceConstant

    val scaled = if (price >= 1) {
        val amount = (BigInt(lo.amount) * lo.price / Order.PriceConstant).bigInteger.longValueExact()
        BigDecimal(amount, priceDecimals)
      } else {
        BigDecimal(lo.amount, amountDecimals)
      }

    (scaled >= 1) :| "Order amount is too small"
  }

  def validateNewOrder(order: Order): Either[CustomError, Order] = {
    //(openOrdersCount.getOrElse(order.matcherPublicKey.address, 0) <= settings.maxOpenOrders) :|
    //  s"Open orders count limit exceeded (Max = ${settings.maxOpenOrders})" &&
    val v =
    (order.matcherPublicKey == matcherPubKey) :| "Incorrect matcher public key" &&
      LimitOrder.validateIntegerAmount(storedState, LimitOrder(order)) &&
      order.isValid(NTP.correctedTime()) &&
      (order.matcherFee >= settings.minOrderFee) :| s"Order matcherFee should be >= ${settings.minOrderFee}" &&
      (orderHistory.getOrderStatus(order.idStr) == LimitOrder.NotFound) :| "Order is already accepted" &&
      isBalanceWithOpenOrdersEnough(order)
    if (!v) {
      Left(CustomError(v.messages()))
    } else {
      Right(order)
    }
  }

  def validateCancelOrder(cancel: CancelOrder): Either[CustomError, CancelOrder] = {
    val status = orderHistory.getOrderStatus(cancel.orderId)
    val v =
      (status != LimitOrder.NotFound) :| "Order not found" &&
        (status != LimitOrder.Filled) :| "Order is already Filled" &&
        cancel.req.isSignatureValid :| "Signature should be valid" &&
        orderHistory.getOrder(cancel.orderId).fold(false)(_.senderPublicKey == cancel.req.senderPublicKey)  :| "Order not found"

    if (!v) {
      Left(CustomError(v.messages()))
    } else {
      Right(cancel)
    }
  }
}
