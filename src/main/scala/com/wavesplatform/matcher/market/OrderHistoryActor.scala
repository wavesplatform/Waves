package com.wavesplatform.matcher.market

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.{BadMatcherResponse, MatcherResponse}
import com.wavesplatform.matcher.market.OrderBookActor.{CancelOrder, GetOrderStatusResponse}
import com.wavesplatform.matcher.market.OrderHistoryActor.{ExpirableOrderHistoryRequest, _}
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model._
import com.wavesplatform.utx.UtxPool
import org.iq80.leveldb.DB
import play.api.libs.json._
import scorex.account.Address
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.transaction.{AssetAcc, AssetId}
import scorex.utils.NTP
import scorex.wallet.Wallet

import scala.concurrent.duration._
import scala.language.postfixOps

class OrderHistoryActor(db: DB, val settings: MatcherSettings, val utxPool: UtxPool, val wallet: Wallet) extends Actor with OrderValidator {

  val orderHistory = OrderHistoryImpl(db, settings)

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[OrderAdded])
    context.system.eventStream.subscribe(self, classOf[OrderExecuted])
    context.system.eventStream.subscribe(self, classOf[OrderCanceled])
  }

  def processExpirableRequest(r: Any): Unit = r match {
    case req: GetOrderHistory =>
      fetchOrderHistory(req)
    case req: GetAllOrderHistory =>
      fetchAllOrderHistory(req)
    case ValidateOrder(o, _) =>
      sender() ! ValidateOrderResult(validateNewOrder(o))
    case ValidateCancelOrder(co, _) =>
      sender() ! ValidateCancelResult(validateCancelOrder(co))
    case req: DeleteOrderFromHistory =>
      deleteFromOrderHistory(req)
    case GetOrderStatus(_, id, _) =>
      sender() ! GetOrderStatusResponse(orderHistory.orderStatus(id))
    case GetTradableBalance(assetPair, addr, _) =>
      sender() ! getPairTradableBalance(assetPair, addr)
    case GetActiveOrdersByAddress(requestId, addr, assets, _) =>
      // Because all orders spend waves for fee
      val wasAssetChanged: Option[AssetId] => Boolean = if (assets.contains(None)) { _ =>
        true
      } else assets.contains

      val allActiveOrders      = orderHistory.activeOrderIdsByAddress(addr.stringRepr)
      val activeOrdersByAssets = allActiveOrders.collect { case (assetId, id) if wasAssetChanged(assetId) => id -> orderHistory.orderInfo(id) }

      val active: Seq[LimitOrder] = activeOrdersByAssets.flatMap {
        case (id, info) =>
          orderHistory.order(id).map { order =>
            LimitOrder(order).partial(info.remaining, info.remainingFee)
          }
      }(collection.breakOut)

      sender().forward(GetActiveOrdersByAddressResponse(requestId, addr, active))
    case GetOpenPortfolio(addr, _) =>
      sender() ! GetOpenPortfolioResponse(OpenPortfolio(orderHistory.openPortfolio(addr).orders.filter(_._2 > 0)))
  }

  override def receive: Receive = {
    case req: ExpirableOrderHistoryRequest =>
      if (NTP.correctedTime() - req.ts < RequestTTL) {
        processExpirableRequest(req)
      }
    case ev: OrderAdded =>
      orderHistory.orderAccepted(ev)
    case ev: OrderExecuted =>
      orderHistory.orderExecuted(ev)
    case ev: OrderCanceled =>
      orderHistory.orderCanceled(ev)
    case RecoverFromOrderBook(ob) =>
      recoverFromOrderBook(ob)
    case ForceCancelOrderFromHistory(id) =>
      forceCancelOrder(id)
  }

  def fetchOrderHistory(req: GetOrderHistory): Unit = {
    sender() ! GetOrderHistoryResponse(orderHistory.fetchOrderHistoryByPair(req.assetPair, req.address))
  }

  def fetchAllOrderHistory(req: GetAllOrderHistory): Unit = {
    if (req.activeOnly) {
      sender() ! GetOrderHistoryResponse(orderHistory.fetchAllActiveOrderHistory(req.address))
    } else {
      sender() ! GetOrderHistoryResponse(orderHistory.fetchAllOrderHistory(req.address))
    }
  }

  def forceCancelOrder(id: String): Unit = {
    orderHistory.order(id).map((_, orderHistory.orderInfo(id))) match {
      case Some((o, oi)) =>
        orderHistory.orderCanceled(OrderCanceled(LimitOrder.limitOrder(o.price, oi.remaining, oi.remainingFee, o), unmatchable = false))
        sender() ! o
      case None =>
        sender() ! None
    }
  }

  def getPairTradableBalance(assetPair: AssetPair, address: String): GetTradableBalanceResponse = {
    val bal = (for {
      acc       <- Address.fromString(address)
      amountAcc <- Right(AssetAcc(acc, assetPair.amountAsset))
      priceAcc  <- Right(AssetAcc(acc, assetPair.priceAsset))
      amountBal <- Right(getTradableBalance(amountAcc))
      priceBal  <- Right(getTradableBalance(priceAcc))
    } yield (amountBal, priceBal)) match {
      case Left(_)  => (0L, 0L)
      case Right(b) => b
    }

    GetTradableBalanceResponse(
      Map(
        assetPair.amountAssetStr -> bal._1,
        assetPair.priceAssetStr  -> bal._2
      ))
  }

  def deleteFromOrderHistory(req: DeleteOrderFromHistory): Unit = {
    orderHistory.orderStatus(req.id) match {
      case LimitOrder.Filled | LimitOrder.Cancelled(_) =>
        orderHistory.deleteOrder(req.address, req.id)
        sender() ! OrderDeleted(req.id)
      case _ =>
        sender() ! BadMatcherResponse(StatusCodes.BadRequest, "Order couldn't be deleted")
    }
  }

  def recoverFromOrderBook(ob: OrderBook): Unit = {
    ob.asks.foreach {
      case (_, orders) =>
        orders.foreach(o => orderHistory.orderAccepted(OrderAdded(o)))
    }
    ob.bids.foreach {
      case (_, orders) =>
        orders.foreach(o => orderHistory.orderAccepted(OrderAdded(o)))
    }
  }

}

object OrderHistoryActor {
  val RequestTTL: Int                          = 5 * 1000
  val UpdateOpenPortfolioDelay: FiniteDuration = 30 seconds

  def name: String = "OrderHistory"

  def props(db: DB, settings: MatcherSettings, utxPool: UtxPool, wallet: Wallet): Props =
    Props(new OrderHistoryActor(db, settings, utxPool, wallet))

  sealed trait OrderHistoryRequest

  sealed trait ExpirableOrderHistoryRequest extends OrderHistoryRequest {
    def ts: Long
  }

  case class GetOrderHistory(assetPair: AssetPair, address: String, ts: Long) extends ExpirableOrderHistoryRequest

  case class GetAllOrderHistory(address: String, activeOnly: Boolean, ts: Long) extends ExpirableOrderHistoryRequest

  case class GetOrderStatus(assetPair: AssetPair, id: String, ts: Long) extends ExpirableOrderHistoryRequest

  case class GetActiveOrdersByAddress(requestId: Long, address: Address, assets: Set[Option[AssetId]], ts: Long) extends ExpirableOrderHistoryRequest

  case class GetActiveOrdersByAddressResponse(requestId: Long, address: Address, orders: Seq[LimitOrder])

  case class DeleteOrderFromHistory(assetPair: AssetPair, address: String, id: String, ts: Long) extends ExpirableOrderHistoryRequest

  case class ValidateOrder(order: Order, ts: Long) extends ExpirableOrderHistoryRequest

  case class ValidateOrderResult(result: Either[GenericError, Order])

  case class ValidateCancelOrder(cancel: CancelOrder, ts: Long) extends ExpirableOrderHistoryRequest

  case class ValidateCancelResult(result: Either[GenericError, CancelOrder])

  case class RecoverFromOrderBook(ob: OrderBook) extends OrderHistoryRequest

  case class ForceCancelOrderFromHistory(orderId: String) extends OrderHistoryRequest

  case class AssetPairAwareResponse(assetPair: AssetPair)

  case object DbCommit

  case class OrderDeleted(orderId: String) extends MatcherResponse {
    val json: JsObject            = Json.obj("status" -> "OrderDeleted", "orderId" -> orderId)
    val code: StatusCodes.Success = StatusCodes.OK
  }

  case class GetOrderHistoryResponse(history: Seq[(String, OrderInfo, Option[Order])]) extends MatcherResponse {
    val json = JsArray(
      history.map(h =>
        Json.obj(
          "id"        -> h._1,
          "type"      -> h._3.map(_.orderType.toString),
          "amount"    -> h._2.amount,
          "price"     -> h._3.map(_.price),
          "timestamp" -> h._3.map(_.timestamp),
          "filled"    -> h._2.filled,
          "status"    -> h._2.status.name,
          "assetPair" -> h._3.map(_.assetPair.json)
      )))
    val code: StatusCode = StatusCodes.OK
  }

  case class GetTradableBalance(assetPair: AssetPair, address: String, ts: Long) extends ExpirableOrderHistoryRequest

  case class GetTradableBalanceResponse(balances: Map[String, Long]) extends MatcherResponse {
    val json: JsObject   = JsObject(balances.map { case (k, v) => (k, JsNumber(v)) })
    val code: StatusCode = StatusCodes.OK
  }

  case class GetOpenPortfolio(address: String, ts: Long) extends ExpirableOrderHistoryRequest

  case class GetOpenPortfolioResponse(portfolio: OpenPortfolio) extends MatcherResponse {
    override def json: JsValue = JsObject(portfolio.orders.map(o => (o._1, JsNumber(o._2))).toSeq)

    override def code: StatusCode = StatusCodes.OK
  }
}
