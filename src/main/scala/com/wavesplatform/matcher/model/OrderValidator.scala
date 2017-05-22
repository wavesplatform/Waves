package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.market.OrderBookActor.CancelOrder
import scorex.account.PublicKeyAccount
import scorex.transaction.AssetAcc
import scorex.transaction.ValidationError.CustomValidationError
import scorex.transaction.assets.exchange.Validation.booleanOperators
import scorex.transaction.assets.exchange.{Order, Validation}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.NTP
import scorex.wallet.Wallet

trait OrderValidator {
  val storedState: StoredState
  val settings: MatcherSettings
  val wallet: Wallet
  val orderHistory: OrderHistory

  lazy val matcherPubKey: PublicKeyAccount = wallet.privateKeyAccount(settings.account).get

  def isBalanceWithOpenOrdersEnough(order: Order): Validation = {
    def notEnoughError(tBal: Long, oBal: Long, needs: Long): String = s"Not enough balance: [$tBal, $oBal], needs: $needs"

    val (acc, feeAcc) = (AssetAcc(order.senderPublicKey, order.getSpendAssetId), AssetAcc(order.senderPublicKey, None))

    val (assTBal, assOBal) = (storedState.tradableAssetBalance(acc), orderHistory.getOpenVolume(acc))
    val (feeTBal, feeOBal) = (storedState.tradableAssetBalance(feeAcc), orderHistory.getOpenVolume(feeAcc))

    if (acc != feeAcc) {
      (assTBal - assOBal >= order.getSpendAmount(order.price, order.amount).get) :|
        notEnoughError(assTBal, assOBal, order.getSpendAmount(order.price, order.amount).get) &&
        (feeTBal - feeOBal >= order.matcherFee) :| notEnoughError(feeTBal, feeOBal, order.matcherFee)
    }
    else {
      (assTBal - assOBal >= order.getSpendAmount(order.price, order.amount).get + order.matcherFee) :|
        notEnoughError(assTBal, assOBal, order.getSpendAmount(order.price, order.amount).get + order.matcherFee)
    }
  }

  def validateNewOrder(order: Order): Either[CustomValidationError, Order] = {
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
      Left(CustomValidationError(v.messages()))
    } else {
      Right(order)
    }
  }

  def validateCancelOrder(cancel: CancelOrder): Either[CustomValidationError, CancelOrder] = {
    val status = orderHistory.getOrderStatus(cancel.orderId)
    val v =
      (status != LimitOrder.NotFound) :| "Order not found" &&
        (status != LimitOrder.Filled) :| "Order is already Filled" &&
        cancel.req.isSignatureValid :| "Signature should be valid" &&
        orderHistory.getOrder(cancel.orderId).fold(false)(_.senderPublicKey == cancel.req.senderPublicKey)  :| "Order not found"

    if (!v) {
      Left(CustomValidationError(v.messages()))
    } else {
      Right(cancel)
    }
  }
}
