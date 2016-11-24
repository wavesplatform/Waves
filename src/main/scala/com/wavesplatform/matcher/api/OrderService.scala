package com.wavesplatform.matcher.api

import com.wavesplatform.matcher.model.OrderItem
import scorex.transaction.AssetAcc
import scorex.transaction.assets.exchange.Validation.BooleanOperators
import scorex.transaction.assets.exchange.{Order, Validation}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.state.database.state._
import scorex.utils.NTP

import scala.collection.mutable

trait OrderService {
  val storedState: StoredState
  val spendAssetToOrders = mutable.Map.empty[Address, Long]

  def getOpenOrdersBalance(assetAcc: AssetAcc): Long = {
    spendAssetToOrders.getOrElse(assetAcc.key, 0L)
  }

  def getOpenOrdersBalance(order: Order): Long = {
    getOpenOrdersBalance(AssetAcc(order.sender, order.spendAssetId))
  }

  def addOpenOrder(orderItem: OrderItem): Unit = {
    val assetAcc = AssetAcc(orderItem.order.sender, orderItem.order.spendAssetId)
    spendAssetToOrders(assetAcc.key) = spendAssetToOrders.getOrElse(assetAcc.key, 0L) + orderItem.sellAmount()
  }

  def removeOpenOrder(orderItem: OrderItem): Unit = {
    val assetAcc = AssetAcc(orderItem.order.sender, orderItem.order.spendAssetId)
    spendAssetToOrders(assetAcc.key) = spendAssetToOrders.getOrElse(assetAcc.key, 0L) - orderItem.sellAmount()
  }

  def removeOpenOrders(orderItem: Seq[OrderItem]): Unit = {
    orderItem.foreach(removeOpenOrder)
  }

  def getBalanceWithOpenOrders(order: Order): Long = {
    val assetAcc = AssetAcc(order.sender, order.spendAssetId)
    storedState.assetBalance(assetAcc) - getOpenOrdersBalance(assetAcc)
  }

  def isBalanceWithOpenOrdersEnough(order: Order): Boolean = {
    getBalanceWithOpenOrders(order) >= order.sellAmount() + order.matcherFee
  }

  def isConfirmedBalanceEnough(orderItem: OrderItem): Boolean = {
    val order = orderItem.order
    val balance = storedState.assetBalance(AssetAcc(order.sender, order.spendAssetId))
    balance >= orderItem.sellAmount() + BigInt(order.matcherFee) * BigInt(orderItem.amount) / BigInt(order.amount)
  }

  def validateNewOrder(order: Order): Validation = {
    order.isValid(NTP.correctedTime()) &&
    isBalanceWithOpenOrdersEnough(order) :| "Not enough balance"
  }

  def validateOrderItem(orderItem: OrderItem): Validation = {
    orderItem.order.isValid(NTP.correctedTime()) &&
      isConfirmedBalanceEnough(orderItem) :| "Not enough balance"
  }
}
