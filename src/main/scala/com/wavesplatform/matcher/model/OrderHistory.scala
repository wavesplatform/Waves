package com.wavesplatform.matcher.model

import cats.implicits._
import com.wavesplatform.db.{AssetIdOrderIdSetCodec, Codec, OrderIdsCodec, PortfolioCodec, SubStorage}
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.model.Events.{Event, OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.LimitOrder.{Filled, OrderStatus}
import com.wavesplatform.matcher.model.OrderHistory.OrderHistoryOrdering
import com.wavesplatform.state2._
import org.iq80.leveldb.DB
import play.api.libs.json.Json
import scorex.transaction.{AssetAcc, AssetId}
import scorex.transaction.assets.exchange.{AssetPair, Order, OrderType}
import scorex.utils.ScorexLogging

trait OrderHistory {
  def orderAccepted(event: OrderAdded): Unit

  def orderExecuted(event: OrderExecuted): Unit

  def orderCanceled(event: OrderCanceled)

  def orderStatus(id: String): OrderStatus

  def orderInfo(id: String): OrderInfo

  def openVolume(assetAcc: AssetAcc): Long

  def openVolumes(address: String): Option[Map[String, Long]]

  def orderIdsByAddress(address: String): Set[String]

  def activeOrderIdsByAddress(address: String): Set[(Option[AssetId], String)]

  def fetchOrderHistoryByPair(assetPair: AssetPair, address: String): Seq[(String, OrderInfo, Option[Order])]

  def fetchAllOrderHistory(address: String): Seq[(String, OrderInfo, Option[Order])]

  def deleteOrder(address: String, orderId: String): Boolean

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

case class OrderHistoryImpl(db: DB, settings: MatcherSettings) extends SubStorage(db: DB, "matcher") with OrderHistory with ScorexLogging {

  private val OrdersPrefix = "orders".getBytes(Charset)
  private val OrdersInfoPrefix = "infos".getBytes(Charset)
  private val AddressToOrdersPrefix = "addr-orders".getBytes(Charset)
  private val AddressToActiveOrdersPrefix = "a-addr-orders".getBytes(Charset)
  private val AddressPortfolioPrefix = "portfolios".getBytes(Charset)

  def savePairAddress(address: String, orderId: String): Unit = {
    get(OrderIdsCodec)(makeKey(AddressToOrdersPrefix, address)) match {
      case Some(prev) =>
        val r = if (prev.length >= settings.maxOrdersPerRequest) {
          val (p1, p2) = prev.span(!orderStatus(_).isFinal)
          if (p2.isEmpty) p1 else p1 ++ p2.tail
        } else prev
        put(makeKey(AddressToOrdersPrefix, address), OrderIdsCodec.encode(r :+ orderId), None)
      case _ =>
        put(makeKey(AddressToOrdersPrefix, address), OrderIdsCodec.encode(Array(orderId)), None)
    }
  }

  private def saveOrderInfo(event: Event): Map[String, (Order, OrderInfo)] = {
    val updatedInfo = Events.createOrderInfo(event).map {
      case (orderId, (o, oi)) => (orderId, (o, orderInfo(orderId).combine(oi)))
    }

    updatedInfo.foreach { case (orderId, (_, oi)) =>
      put(makeKey(OrdersInfoPrefix, orderId), oi.jsonStr.getBytes(Charset), None)
      log.debug(s"Changed OrderInfo for: $orderId -> $oi")
    }

    updatedInfo
  }

  def openPortfolio(address: String): OpenPortfolio = {
    get(PortfolioCodec)(makeKey(AddressPortfolioPrefix, address)).map(OpenPortfolio.apply).getOrElse(OpenPortfolio.empty)
  }

  def saveOpenPortfolio(event: Event): Unit = {
    Events.createOpenPortfolio(event).foreach { case (address, op) =>
      val key = makeKey(AddressPortfolioPrefix, address)
      val prev = get(PortfolioCodec)(key).map(OpenPortfolio.apply).getOrElse(OpenPortfolio.empty)
      val updatedPortfolios = prev.combine(op)
      put(key, PortfolioCodec.encode(updatedPortfolios.orders), None)
      log.debug(s"Changed OpenPortfolio for: $address -> " + updatedPortfolios.toString)
    }
  }

  def saveOrder(order: Order): Unit = {
    val key = makeKey(OrdersPrefix, order.idStr())
    if (get(key).isEmpty)
      put(key, order.jsonStr.getBytes(Charset), None)
  }

  def deleteFromOrders(orderId: String): Unit = {
    delete(makeKey(OrdersPrefix, orderId), None)
  }

  override def orderAccepted(event: OrderAdded): Unit = {
    val lo = event.order
    saveOrder(lo.order)
    val updatedInfo = saveOrderInfo(event)
    saveOpenPortfolio(event)
    savePairAddress(lo.order.senderPublicKey.address, lo.order.idStr())

    updatedInfo.foreach { case (orderId, (o, oi)) =>
      if (!oi.status.isFinal) {
        val assetId = if (o.orderType == OrderType.BUY) o.assetPair.priceAsset else o.assetPair.amountAsset
        addToActive(o.senderPublicKey.address, assetId, orderId)
      }
    }
  }

  override def orderExecuted(event: OrderExecuted): Unit = {
    saveOrder(event.submitted.order)
    savePairAddress(event.submitted.order.senderPublicKey.address, event.submitted.order.idStr())
    val updatedInfo = saveOrderInfo(event)
    saveOpenPortfolio(OrderAdded(event.submittedExecuted))
    saveOpenPortfolio(event)

    updatedInfo.foreach { case (orderId, (o, oi)) =>
      if (oi.status.isFinal) deleteFromActive(o.senderPublicKey.address, orderId)
    }
  }

  override def orderCanceled(event: OrderCanceled): Unit = {
    val updatedInfo = saveOrderInfo(event)
    saveOpenPortfolio(event)

    updatedInfo.foreach { case (orderId, (o, oi)) =>
      if (oi.status.isFinal) deleteFromActive(o.senderPublicKey.address, orderId)
    }
  }

  override def orderInfo(id: String): OrderInfo =
    get(makeKey(OrdersInfoPrefix, id)).map(Json.parse).flatMap(_.validate[OrderInfo].asOpt).getOrElse(OrderInfo.empty)

  override def order(id: String): Option[Order] = {
    import scorex.transaction.assets.exchange.OrderJson.orderFormat
    get(makeKey(OrdersPrefix, id)).map(b => new String(b, Charset)).map(Json.parse).flatMap(_.validate[Order].asOpt)
  }

  override def orderStatus(id: String): OrderStatus = {
    orderInfo(id).status
  }

  override def openVolume(assetAcc: AssetAcc): Long = {
    val asset = assetAcc.assetId.map(_.base58).getOrElse(AssetPair.WavesName)
    get(PortfolioCodec)(makeKey(AddressPortfolioPrefix, assetAcc.account.address))
      .flatMap(_.get(asset)).map(math.max(0L, _)).getOrElse(0L)
  }

  override def openVolumes(address: String): Option[Map[String, Long]] = {
    get(PortfolioCodec)(makeKey(AddressPortfolioPrefix, address))
  }

  override def orderIdsByAddress(address: String): Set[String] = {
    get(OrderIdsCodec)(makeKey(AddressToOrdersPrefix, address)).map(_.toSet).getOrElse(Set.empty)
  }

  override def activeOrderIdsByAddress(address: String): Set[(Option[AssetId], String)] = {
    get(AssetIdOrderIdSetCodec)(makeKey(AddressToActiveOrdersPrefix, address)).getOrElse(Set.empty)
  }

  override def deleteOrder(address: String, orderId: String): Boolean = {
    orderStatus(orderId) match {
      case Filled | LimitOrder.Cancelled(_) =>
        deleteFromOrders(orderId)
        deleteFromOrdersInfo(orderId)
        deleteFromAddress(address, orderId)
        deleteFromActive(address, orderId)
        true
      case _ =>
        false
    }
  }

  override def fetchOrderHistoryByPair(assetPair: AssetPair, address: String): Seq[(String, OrderInfo, Option[Order])] = {
    orderIdsByAddress(address)
      .toSeq
      .map(id => (id, orderInfo(id), order(id)))
      .filter(_._3.exists(_.assetPair == assetPair))
      .sorted(OrderHistoryOrdering)
      .take(settings.maxOrdersPerRequest)
  }

  override def fetchAllOrderHistory(address: String): Seq[(String, OrderInfo, Option[Order])] = {
    import OrderInfo.orderStatusOrdering
    orderIdsByAddress(address)
      .toSeq
      .map(id => (id, orderInfo(id)))
      .sortBy(_._2.status)
      .take(settings.maxOrdersPerRequest)
      .map(p => (p._1, p._2, order(p._1)))
      .sorted(OrderHistoryOrdering)
  }

  private def deleteFromOrdersInfo(orderId: String): Unit = delete(makeKey(OrdersInfoPrefix, orderId), None)

  private def deleteFromAddress(address: String, orderId: String): Unit = {
    deleteOrderFromAddress(AddressToOrdersPrefix, address, orderId)
  }

  private def addToActive(address: String, assetId: Option[AssetId], orderId: String): Unit =  {
    val key = makeKey(AddressToActiveOrdersPrefix, address)
    val orig = get(AssetIdOrderIdSetCodec)(key).getOrElse(Set.empty)
    put(key, AssetIdOrderIdSetCodec.encode(orig + (assetId -> orderId)), None)
  }

  private def deleteFromActive(address: String, orderId: String): Unit = {
    update(AssetIdOrderIdSetCodec)(makeKey(AddressToActiveOrdersPrefix, address)) { orig =>
      orig.filterNot { case (_, currOrderId) => currOrderId == orderId }
    }
  }

  private def deleteOrderFromAddress(keyPrefix: Array[Byte], address: String, orderId: String): Unit = {
    update(OrderIdsCodec)(makeKey(keyPrefix, address)) { orig =>
      if (orig.contains(orderId)) orig.filterNot(_ == orderId)
      else orig
    }
  }

  private def update[Content](codec: Codec[Content])(key: Array[Byte])(f: Content => Content): Unit = {
    get(codec)(key).foreach { orig => put(key, codec.encode(f(orig)), None) }
  }

  private def get[Content](codec: Codec[Content])(key: Array[Byte]): Option[Content] = {
    get(key).map { x => codec.decode(x).explicitGet().value }
  }
}
