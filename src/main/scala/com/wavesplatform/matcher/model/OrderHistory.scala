package com.wavesplatform.matcher.model

import cats.implicits._
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.LimitOrder.{Filled, OrderStatus}
import com.wavesplatform.matcher.model.OrderHistory.OrderHistoryOrdering
import play.api.libs.json.Json
import scorex.transaction.AssetAcc
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.utils.ScorexLogging

trait OrderHistory {
  def orderAccepted(event: OrderAdded): Unit
  def orderExecuted(event: OrderExecuted): Unit
  def orderCanceled(event: OrderCanceled)
  def orderStatus(id: String): OrderStatus
  def orderInfo(id: String): OrderInfo
  def openVolume(assetAcc: AssetAcc): Long

  def openVolumes(address: String): Option[Map[String, Long]]
  def ordersByPairAndAddress(assetPair: AssetPair, address: String): Set[String]
  def getAllOrdersByAddress(address: String): Set[String]
  def fetchOrderHistoryByPair(assetPair: AssetPair, address: String): Seq[(String, OrderInfo, Option[Order])]
  def fetchAllOrderHistory(address: String): Seq[(String, OrderInfo, Option[Order])]
  def deleteOrder(assetPair: AssetPair, address: String, orderId: String): Boolean
  def order(id: String): Option[Order]
  def openPortfolio(address: String): OpenPortfolio
}

object OrderHistory {
  import OrderInfo.orderStatusOrdering
  object OrderHistoryOrdering extends Ordering[(String, OrderInfo, Option[Order])] {
    def orderBy(oh: (String, OrderInfo, Option[Order])): (OrderStatus, Long) = (oh._2.status, -oh._3.map(_.timestamp).getOrElse(0L))
    override def compare(first: (String, OrderInfo, Option[Order]), second: (String, OrderInfo, Option[Order])): Int = {
      implicitly[Ordering[(OrderStatus, Long)]].compare(orderBy(first), orderBy(second))
    }
  }
}

case class OrderHistoryImpl(p: OrderHistoryStorage, settings: MatcherSettings) extends OrderHistory with ScorexLogging {

  def savePairAddress(assetPair: AssetPair, address: String, orderId: String): Unit = {
    Option(p.addressToOrderIds.get(address)) match {
      case Some(prev) =>
        var r = prev
        if (prev.length >= settings.maxOrdersPerAddress) {
          val (p1, p2) = prev.span(!orderStatus(_).isInstanceOf[LimitOrder.Cancelled])
          r = if (p2.isEmpty) p1 else p1 ++ p2.tail
        }
        p.addressToOrderIds.put(address, r :+ orderId)
      case _ =>
        p.addressToOrderIds.put(address, Array(orderId))
    }
  }

  def saveOrdeInfo(event: Event): Unit = {
    Events.createOrderInfo(event).foreach{ case(orderId, oi) =>
      p.ordersInfo.put(orderId, orderInfo(orderId).combine(oi))
      log.debug(s"Changed OrderInfo for: $orderId -> " + orderInfo(orderId))
    }
  }

  def openPortfolio(address: String): OpenPortfolio = {
    Option(p.addressToOrderPortfolio.get(address)).map(OpenPortfolio(_)).orEmpty
  }

  def saveOpenPortfolio(event: Event): Unit = {
    Events.createOpenPortfolio(event).foreach{ case(addr, op) =>
      val prev = Option(p.addressToOrderPortfolio.get(addr)).map(OpenPortfolio(_)).getOrElse(OpenPortfolio.empty)
      p.addressToOrderPortfolio.put(addr, prev.combine(op).orders)
      log.debug(s"Changed OpenPortfolio for: $addr -> " + p.addressToOrderPortfolio.get(addr).toString)
    }
  }

  def saveOrder(order: Order): Unit = {
    if (!p.orders.containsKey(order.idStr())) {
      p.orders.putIfAbsent(order.idStr(), order.jsonStr)
    }
  }

  def deleteFromOrders(orderId: String): Unit = {
    p.orders.remove(orderId)
  }

  override def orderAccepted(event: OrderAdded): Unit = {
    val lo = event.order
    saveOrder(lo.order)
    saveOrdeInfo(event)
    saveOpenPortfolio(event)
    savePairAddress(lo.order.assetPair, lo.order.senderPublicKey.address, lo.order.idStr())
  }

  override def orderExecuted(event: OrderExecuted): Unit = {
    saveOrder(event.submitted.order)
    savePairAddress(event.submitted.order.assetPair, event.submitted.order.senderPublicKey.address, event.submitted.order.idStr())
    saveOrdeInfo(event)
    saveOpenPortfolio(OrderAdded(event.submittedExecuted))
    saveOpenPortfolio(event)
  }

  override def orderCanceled(event: OrderCanceled): Unit = {
    saveOrdeInfo(event)
    saveOpenPortfolio(event)
  }

  override def orderInfo(id: String): OrderInfo =
    Option(p.ordersInfo.get(id)).getOrElse(OrderInfo.empty)

  override def order(id: String): Option[Order] = {
    import scorex.transaction.assets.exchange.OrderJson.orderFormat
    Option(p.orders.get(id)).map(Json.parse).flatMap(_.validate[Order].asOpt)
  }

  override def orderStatus(id: String): OrderStatus = {
    orderInfo(id).status
  }

  override def openVolume(assetAcc: AssetAcc): Long = {
    val asset = assetAcc.assetId.map(_.base58).getOrElse(AssetPair.WavesName)
    Option(p.addressToOrderPortfolio.get(assetAcc.account.address)).flatMap(_.get(asset)).map(math.max(0L, _)).getOrElse(0L)
  }

  override def openVolumes(address: String): Option[Map[String, Long]] = Option(p.addressToOrderPortfolio.get(address))

  override def ordersByPairAndAddress(assetPair: AssetPair, address: String): Set[String] = {
    Option(p.addressToOrderIds.get(address)).map(_.toSet).getOrElse(Set())
  }

  override def fetchOrderHistoryByPair(assetPair: AssetPair, address: String): Seq[(String, OrderInfo, Option[Order])] = {
    getAllOrdersByAddress(address)
      .toSeq
      .map(id => (id, orderInfo(id), order(id)))
      .filter(_._3.exists(_.assetPair == assetPair))
      .sorted(OrderHistoryOrdering)
      .take(settings.maxOrdersPerRequest)
  }

  override def getAllOrdersByAddress(address: String): Set[String] = {
    Option(p.addressToOrderIds.get(address)).map(_.toSet).getOrElse(Set())
  }

  override def fetchAllOrderHistory(address: String): Seq[(String, OrderInfo, Option[Order])] = {
    import OrderInfo.orderStatusOrdering
    getAllOrdersByAddress(address)
      .toSeq
      .map(id => (id, orderInfo(id)))
      .sortBy(_._2.status)
      .take(settings.maxOrdersPerRequest)
      .map(p => (p._1, p._2, order(p._1)))
      .sorted(OrderHistoryOrdering)
  }

  private def deleteFromOrdersInfo(orderId: String): Unit = {
    p.ordersInfo.remove(orderId)
  }

  private def deleteFromPairAddress(assetPair: AssetPair, address: String, orderId: String): Unit = {
    Option(p.addressToOrderIds.get(address)) match {
      case Some(prev) =>
        if (prev.contains(orderId)) p.addressToOrderIds.put(address, prev.filterNot(_ == orderId))
      case _ =>
    }
  }

  override def deleteOrder(assetPair: AssetPair, address: String, orderId: String): Boolean = {
    orderStatus(orderId) match {
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
