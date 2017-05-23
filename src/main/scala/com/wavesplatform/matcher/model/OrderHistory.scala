package com.wavesplatform.matcher.model

import cats.implicits._
import com.wavesplatform.matcher.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.LimitOrder.{Filled, OrderStatus}
import play.api.libs.json.Json
import scorex.crypto.encode.Base58
import scorex.transaction.AssetAcc
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.utils.ScorexLogging

trait OrderHistory {
  def didOrderAccepted(event: OrderAdded): Unit
  def didOrderExecuted(event: OrderExecuted): Unit
  def didOrderCanceled(event: OrderCanceled)
  def getOrderStatus(id: String): OrderStatus
  def getOrderInfo(id: String): OrderInfo
  def getOpenVolume(assetAcc: AssetAcc): Long
  def getOrdersByPairAndAddress(assetPair: AssetPair, address: String): Set[String]
  def deleteOrder(assetPair: AssetPair, address: String, orderId: String): Boolean
  def getOrder(id: String): Option[Order]
}

case class OrderHistoryImpl(p: OrderHistoryStorage) extends OrderHistory with ScorexLogging {

  def savePairAddress(assetPair: AssetPair, address: String, orderId: String): Unit = {
    val pairAddress = OrderHistoryStorage.assetPairAddressKey(assetPair, address)
    Option(p.pairAddressToOrderIds.get(pairAddress)) match {
      case Some(prev) =>
        if (!prev.contains(orderId)) p.pairAddressToOrderIds.put(pairAddress, prev + orderId)
      case _ =>
        p.pairAddressToOrderIds.put(pairAddress, Set(orderId))
    }
  }

  def saveOrdeInfo(event: Event): Unit = {
    Events.createOrderInfo(event).foreach{ case(orderId, oi) =>
      p.ordersInfo.put(orderId, getOrderInfo(orderId).combine(oi).jsonStr)
      log.debug(s"Changed OrderInfo for: $orderId -> " + getOrderInfo(orderId))
    }
  }

  def saveOpenPortfolio(event: Event): Unit = {
    Events.createOpenPortfolio(event).foreach{ case(addr, op) =>
      val prev = Option(p.addressToOrderPortfolio.get(addr)).map(OpenPortfolio(_)).getOrElse(OpenPortfolio.empty)
      p.addressToOrderPortfolio.put(addr, prev.combine(op).orders)
      log.debug(s"Changed OpenPortfolio for: $addr -> " + p.addressToOrderPortfolio.get(addr).toString)
    }
  }

  def saveOrder(order: Order): Unit = {
    if (!p.orders.containsKey(order.idStr)) {
      p.orders.putIfAbsent(order.idStr, order.jsonStr)
    }
  }

  def deleteFromOrders(orderId: String): Unit = {
    p.orders.remove(orderId)
  }

  override def didOrderAccepted(event: OrderAdded): Unit = {
    val lo = event.order
    saveOrder(lo.order)
    saveOrdeInfo(event)
    saveOpenPortfolio(event)
    savePairAddress(lo.order.assetPair, lo.order.senderPublicKey.address, lo.order.idStr)
  }

  override def didOrderExecuted(event: OrderExecuted): Unit = {
    saveOrder(event.submitted.order)
    savePairAddress(event.submitted.order.assetPair, event.submitted.order.senderPublicKey.address, event.submitted.order.idStr)
    saveOrdeInfo(event)
    saveOpenPortfolio(event)
  }

  override def didOrderCanceled(event: OrderCanceled): Unit = {
    saveOrdeInfo(event)
    saveOpenPortfolio(event)
  }

  override def getOrderInfo(id: String): OrderInfo =
    Option(p.ordersInfo.get(id)).map(Json.parse).flatMap(_.validate[OrderInfo].asOpt).getOrElse(OrderInfo.empty)

  override def getOrder(id: String): Option[Order] = {
    import scorex.transaction.assets.exchange.OrderJson.orderFormat
    Option(p.orders.get(id)).map(Json.parse).flatMap(_.validate[Order].asOpt)
  }

  override def getOrderStatus(id: String): OrderStatus = {
    getOrderInfo(id).status
  }

  override def getOpenVolume(assetAcc: AssetAcc): Long = {
    val asset = assetAcc.assetId.map(Base58.encode).getOrElse(AssetPair.WavesName)
    Option(p.addressToOrderPortfolio.get(assetAcc.account.address)).flatMap(_.get(asset)).getOrElse(0L)
  }

  override def getOrdersByPairAndAddress(assetPair: AssetPair, address: String): Set[String] = {
    val pairAddressKey = OrderHistoryStorage.assetPairAddressKey(assetPair, address)
    Option(p.pairAddressToOrderIds.get(pairAddressKey)).getOrElse(Set())
  }

  private def deleteFromOrdersInfo(orderId: String): Unit = {
    p.ordersInfo.remove(orderId)
  }

  private def deleteFromPairAddress(assetPair: AssetPair, address: String, orderId: String): Unit = {
    val pairAddress = OrderHistoryStorage.assetPairAddressKey(assetPair, address)
    Option(p.pairAddressToOrderIds.get(pairAddress)) match {
      case Some(prev) =>
        if (prev.contains(orderId)) p.pairAddressToOrderIds.put(pairAddress, prev - orderId)
      case _ =>
    }
  }

  override def deleteOrder(assetPair: AssetPair, address: String, orderId: String): Boolean = {
    getOrderStatus(orderId) match {
      case Filled | LimitOrder.Cancelled(_) =>
        deleteFromOrders(orderId)
        deleteFromOrdersInfo(orderId)
        deleteFromPairAddress(assetPair, address, orderId)
        true
      case _ =>
        false
    }
  }
}
