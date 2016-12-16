package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.MatcherSettings
import scorex.transaction.AssetAcc
import scorex.transaction.assets.exchange.Validation.BooleanOperators
import scorex.transaction.assets.exchange.{Order, OrderCancelTransaction, Validation}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.NTP

trait OrderValidator {
  this: OrderHistory =>
  val storedState: StoredState
  val settings: MatcherSettings

  def isBalanceWithOpenOrdersEnough(order: Order): Boolean = {
    val (acc, feeAcc) = (AssetAcc(order.sender, order.spendAssetId), AssetAcc(order.sender, None))

    val (assBal, feeBal) = (storedState.assetBalance(acc) - assetsToSpend.getOrElse(acc.key, 0L),
      storedState.assetBalance(feeAcc) - assetsToSpend.getOrElse(feeAcc.key, 0L))

    if (acc != feeAcc) assBal >= order.getSpendAmount && feeBal >= order.matcherFee
    else assBal >= order.getSpendAmount + order.matcherFee
  }

  def validateNewOrder(order: Order): Validation = {
    (openOrdersCount.getOrElse(order.sender.address, 0) <= settings.maxOpenOrdersCount) :|
      s"Open orders count limit exceeded (Max = ${settings.maxOpenOrdersCount})" &&
      order.isValid(NTP.correctedTime()) &&
      (order.matcherFee >= settings.minOrderFee) :| s"Order matcherFee should be >= ${settings.minOrderFee}" &&
      !ordersRemainingAmount.contains(order.idStr) :| "Order is already accepted" &&
      isBalanceWithOpenOrdersEnough(order) :| "Not enough balance"
  }

  def validateCancelOrder(cancel: OrderCancelTransaction): Validation = {
    ordersRemainingAmount.contains(cancel.orderIdStr) :| "Order not found" &&
      cancel.isValid &&
      (cancel.fee >= settings.minCancelOrderFee) :| s"Cancel Order fee should be >= ${settings.minCancelOrderFee}" &&
      (getOrderStatus(cancel.orderIdStr) != LimitOrder.Filled) :| "Order is already Filled"
  }
}
