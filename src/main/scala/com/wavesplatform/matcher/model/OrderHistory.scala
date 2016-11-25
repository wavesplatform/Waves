package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.model.OrderItem.OrderStatus
import com.wavesplatform.matcher.util.{Cache, TTLCache}
import scorex.transaction.AssetAcc
import scorex.transaction.state.database.state._

import scala.collection.mutable
import scala.concurrent.duration._

trait OrderHistory {
  val assetsToSpend = mutable.Map.empty[Address, Long]
  val ordersRemainingAmount: Cache[String, (Long, Long)] = TTLCache[String, (Long, Long)](1.minutes)

  def addOpenOrder(orderItem: OrderItem): Unit = {
    val order = orderItem.order
    val assetAcc = AssetAcc(order.sender, order.spendAssetId)
    val feeAssetAcc = AssetAcc(order.sender, None)
    assetsToSpend(assetAcc.key) = assetsToSpend.getOrElse(assetAcc.key, 0L) + orderItem.sellAmount()
    assetsToSpend(feeAssetAcc.key) = assetsToSpend.getOrElse(feeAssetAcc.key, 0L) + orderItem.feeAmount()

    ordersRemainingAmount.set(order.idStr, (order.amount, orderItem.amount))
  }

  def removeOrderItems(orderItems: Seq[OrderItem]): Unit = {
    orderItems.foreach(removeOrderItem)
  }

  def removeOrderItem(orderItem: OrderItem): Unit = {
    def reduceSpendAssets(key: Address, value: Long) =
      if (assetsToSpend.contains(key)) {
        val newVal = assetsToSpend(key) - value
        if (newVal > 0) assetsToSpend(key) = newVal
        else assetsToSpend -= key
      }

    val order = orderItem.order
    val assetAcc = AssetAcc(order.sender, order.spendAssetId)
    val feeAssetAcc = AssetAcc(order.sender, None)

    reduceSpendAssets(assetAcc.key, orderItem.sellAmount())
    reduceSpendAssets(feeAssetAcc.key,  orderItem.feeAmount())

    ordersRemainingAmount.get(order.idStr).foreach(prev =>
      ordersRemainingAmount.set(order.idStr, (order.amount, prev._2 - orderItem.amount))
    )
  }

  def getOrderStatus(id: String): OrderStatus = {
    if (!ordersRemainingAmount.contains(id))
      OrderItem.NotFound
    else {
      val (full, exec) = ordersRemainingAmount.get(id).get
      if (exec == 0) OrderItem.Accepted
      else if (exec < full) OrderItem.PartiallyFilled(exec)
      else OrderItem.Filled
    }
  }
}
