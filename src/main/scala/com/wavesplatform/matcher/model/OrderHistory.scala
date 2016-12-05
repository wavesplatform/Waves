package com.wavesplatform.matcher.model

import com.wavesplatform.matcher.model.LimitOrder.OrderStatus
import com.wavesplatform.matcher.util.{Cache, TTLCache}
import scorex.transaction.AssetAcc
import scorex.transaction.state.database.state._

import scala.collection.mutable
import scala.concurrent.duration._

trait OrderHistory {
  val assetsToSpend = mutable.Map.empty[Address, Long]
  var ordersRemainingAmount: Cache[String, (Long, Long)] = TTLCache[String, (Long, Long)](1.minutes)

  def addOpenOrder(limitOrder: LimitOrder): Unit = {
    val order = limitOrder.order
    val assetAcc = AssetAcc(order.sender, order.spendAssetId)
    val feeAssetAcc = AssetAcc(order.sender, None)
    assetsToSpend(assetAcc.key) = assetsToSpend.getOrElse(assetAcc.key, 0L) + limitOrder.sellAmount
    assetsToSpend(feeAssetAcc.key) = assetsToSpend.getOrElse(feeAssetAcc.key, 0L) + limitOrder.feeAmount

    ordersRemainingAmount.set(order.idStr, (order.amount, limitOrder.amount))
  }

  def removeOrderItems(orderItems: Seq[LimitOrder]): Unit = {
    orderItems.foreach(removeOrderItem)
  }

  def removeOrderItem(limitOrder: LimitOrder): Unit = {
    def reduceSpendAssets(key: Address, value: Long) =
      if (assetsToSpend.contains(key)) {
        val newVal = assetsToSpend(key) - value
        if (newVal > 0) assetsToSpend(key) = newVal
        else assetsToSpend -= key
      }

    val order = limitOrder.order
    val assetAcc = AssetAcc(order.sender, order.spendAssetId)
    val feeAssetAcc = AssetAcc(order.sender, None)

    reduceSpendAssets(assetAcc.key, limitOrder.sellAmount)
    reduceSpendAssets(feeAssetAcc.key,  limitOrder.feeAmount)

    ordersRemainingAmount.get(order.idStr).foreach(prev =>
      ordersRemainingAmount.set(order.idStr, (order.amount, prev._2 - limitOrder.amount))
    )
  }

  def getOrderStatus(id: String): OrderStatus = {
    if (!ordersRemainingAmount.contains(id))
      LimitOrder.NotFound
    else {
      val (full, exec) = ordersRemainingAmount.get(id).get
      if (exec == 0) LimitOrder.Accepted
      else if (exec < full) LimitOrder.PartiallyFilled(exec)
      else LimitOrder.Filled
    }
  }
}
