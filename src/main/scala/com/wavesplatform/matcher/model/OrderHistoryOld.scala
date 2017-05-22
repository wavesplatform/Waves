package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.model.Events.{OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.LimitOrder.OrderStatus
import com.wavesplatform.matcher.util.{Cache, TTLCache}
import scorex.crypto.encode.Base58
import scorex.transaction.AssetAcc
import scorex.transaction.assets.exchange.Order
import scorex.transaction.state.database.state.{Address, _}

import scala.collection.mutable
import scala.concurrent.duration._

trait OrderHistoryOld {
  val assetsToSpend = mutable.Map.empty[Address, Long]
  val ordersRemainingAmount: Cache[String, (Long, Long)] =
    TTLCache[String, (Long, Long)]((Order.MaxLiveTime + 3600*1000).millis)
  val openOrdersCount = mutable.Map.empty[Address, Int]
  val addressToOrders = mutable.Map.empty[String, Seq[String]]


  def initOrdersCache(m: Map[String, (Long, Long)], a: Map[Address, Seq[String]]) = {
    ordersRemainingAmount.clear()
    m.foreach(v => ordersRemainingAmount.set(v._1, v._2))
    a.foreach(v => addressToOrders.put(v._1, v._2))
  }

  private def incCount(address: Address) = openOrdersCount(address) = openOrdersCount.getOrElse(address, 0) + 1
  private def decCount(address: Address) = openOrdersCount(address) = openOrdersCount.getOrElse(address, 0) - 1

  private def addAssetsToSpend(lo: LimitOrder) = {
    val order = lo.order
    val assetAcc = AssetAcc(order.senderPublicKey, order.getSpendAssetId)
    val feeAssetAcc = AssetAcc(order.senderPublicKey, None)
    assetsToSpend(assetAcc.key) = assetsToSpend.getOrElse(assetAcc.key, 0L) + lo.getSpendAmount
    assetsToSpend(feeAssetAcc.key) = assetsToSpend.getOrElse(feeAssetAcc.key, 0L) + lo.feeAmount

    incCount(order.senderPublicKey.address)
    val publicKey = Base58.encode(lo.order.senderPublicKey.publicKey)
    val prev = addressToOrders.getOrElse(publicKey, Vector())
    if (!prev.contains(lo.order.idStr))
      addressToOrders(publicKey) = prev :+ lo.order.idStr
  }

  private def updateRemaining(orderId: String, d: (Long, Long)) = {
    val prev = ordersRemainingAmount.get(orderId).getOrElse(0L, 0L)
    ordersRemainingAmount.set(orderId, (prev._1 + d._1 , prev._2 + d._2))
  }

  def didOrderAccepted(lo: LimitOrder): Unit = {
    addAssetsToSpend(lo)

    updateRemaining(lo.order.idStr, (lo.amount, 0L))
  }

  def deleteOrder(publicKey: String, orderId: String): Unit = {
    addressToOrders(publicKey) = addressToOrders.getOrElse(publicKey, Vector()).filterNot(_ == orderId)
  }

  def reduceSpendAssets(limitOrder: LimitOrder) = {
    def reduce(key: Address, value: Long) =
      if (assetsToSpend.contains(key)) {
        val newVal = assetsToSpend(key) - value
        if (newVal > 0) assetsToSpend += (key -> newVal)
        else assetsToSpend -= key
      }

    val order = limitOrder.order
    val assetAcc = AssetAcc(order.senderPublicKey, order.getSpendAssetId)
    val feeAssetAcc = AssetAcc(order.senderPublicKey, None)

    reduce(assetAcc.key, limitOrder.getSpendAmount)
    reduce(feeAssetAcc.key, limitOrder.feeAmount)
  }


  def didOrderExecuted(e: OrderExecuted): Unit = {
    reduceSpendAssets(e.counterExecuted)

    updateRemaining(e.submitted.order.idStr, (e.executedAmount, e.executedAmount))
    updateRemaining(e.counter.order.idStr, (0L, e.executedAmount))

    if (e.isCounterFilled) decCount(e.counterExecuted.order.senderPublicKey.address)
  }

  def didOrderCanceled(orderCanceled: OrderCanceled): Unit = {
    val o = orderCanceled.limitOrder.order
    updateRemaining(o.idStr, (-o.amount, 0L))

    reduceSpendAssets(orderCanceled.limitOrder)

    decCount(o.senderPublicKey.address)
  }

  def getOrderStatus(id: String): OrderStatus = {
    if (!ordersRemainingAmount.contains(id))
      LimitOrder.NotFound
    else {
      val (full, filled) = ordersRemainingAmount.get(id).get
      if (full == 0) LimitOrder.Cancelled(filled)
      else if (filled == 0) LimitOrder.Accepted
      else if (filled < full) LimitOrder.PartiallyFilled(filled)
      else LimitOrder.Filled
    }
  }

  def getOrderHistory(address: String): Seq[(String, OrderStatus, Long, Long)] = {
    addressToOrders.get(address).map {orders =>
      orders.map { o =>
        val (amount, filled) = ordersRemainingAmount.get(o).getOrElse((0L, 0L))
        (o, getOrderStatus(o), amount, filled)
      }
    }.getOrElse(Seq())
  }
}
