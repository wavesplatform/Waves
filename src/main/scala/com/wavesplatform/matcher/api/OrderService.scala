package com.wavesplatform.matcher.api

import scorex.transaction.AssetAcc
import scorex.transaction.assets.exchange.Validation.BooleanOperators
import scorex.transaction.assets.exchange.{Order, Validation}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.NTP

trait OrderService {
  val storedState: StoredState

  def isBalanceEnough(order: Order): Boolean = {
    val balance = storedState.assetBalance(AssetAcc(order.sender, Some(order.spendAssetId)))
    balance >= order.sellAmount()
  }

  def validateOrder(order: Order): Validation = {
    order.isValid(NTP.correctedTime()) &&
    isBalanceEnough(order) :| "Not enough balance"
  }

}
