package com.wavesplatform.matcher.model

import scorex.transaction.AssetAcc
import scorex.transaction.assets.exchange.Validation.BooleanOperators
import scorex.transaction.assets.exchange.{Order, Validation}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.NTP

trait OrderValidator {
  this: OrderHistory =>
  val storedState: StoredState

  def isBalanceWithOpenOrdersEnough(order: Order): Boolean = {
    val (acc, feeAcc) = (AssetAcc(order.sender, order.spendAssetId), AssetAcc(order.sender, None))

    val (assBal, feeBal) = (storedState.assetBalance(acc) - assetsToSpend.getOrElse(acc.key, 0L),
      storedState.assetBalance(feeAcc) - assetsToSpend.getOrElse(feeAcc.key, 0L))

    if (acc != feeAcc) assBal >= order.sellAmount() && feeBal >= order.matcherFee
    else assBal >= order.sellAmount() + order.matcherFee
  }

  def validateNewOrder(order: Order): Validation = {
    order.isValid(NTP.correctedTime()) &&
    ordersRemainingAmount.contains(order.idStr) :| "Order is already accepted"
    isBalanceWithOpenOrdersEnough(order) :| "Not enough balance"
  }

}
