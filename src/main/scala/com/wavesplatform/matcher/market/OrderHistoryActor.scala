package com.wavesplatform.matcher.market

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.{BadMatcherResponse, MatcherResponse}
import com.wavesplatform.matcher.market.OrderBookActor.{CancelOrder, GetOrderStatusResponse}
import com.wavesplatform.matcher.market.OrderHistoryActor._
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.LimitOrder.Filled
import com.wavesplatform.matcher.model._
import com.wavesplatform.{UtxPool, utils}
import org.h2.mvstore.MVStore
import play.api.libs.json._
import scorex.account.Address
import scorex.transaction.AssetAcc
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.utils.NTP
import scorex.wallet.Wallet

import scala.concurrent.duration._
import scala.language.postfixOps

class OrderHistoryActor(val settings: MatcherSettings, val utxPool: UtxPool, val wallet: Wallet)
  extends Actor with OrderValidator {

  val db: MVStore = utils.createStore(settings.orderHistoryFile)
  val storage = new OrderHistoryStorage(db)
  val orderHistory = OrderHistoryImpl(storage)

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[OrderAdded])
    context.system.eventStream.subscribe(self, classOf[OrderExecuted])
    context.system.eventStream.subscribe(self, classOf[OrderCanceled])
  }

  override def postStop(): Unit = {
    db.commit()
    db.close()
  }

  def processExpirableRequest(r: Any): Unit = r match {
    case req: GetOrderHistory =>
      fetchOrderHistory(req)
    case req: GetAllOrderHistory =>
      fetchAllOrderHistory(req)
    case ValidateOrder(o, ts) =>
      sender() ! ValidateOrderResult(validateNewOrder(o))
    case ValidateCancelOrder(co, _) =>
      sender() ! ValidateCancelResult(validateCancelOrder(co))
    case req: DeleteOrderFromHistory =>
      deleteFromOrderHistory(req)
    case GetOrderStatus(_, id, _) =>
      sender() ! GetOrderStatusResponse(orderHistory.orderStatus(id))
    case GetTradableBalance(assetPair, addr, _) =>
      sender() ! getPairTradableBalance(assetPair, addr)
    case GetMatcherBalance(addr, _) =>
      sender() ! getMatcherBalance(addr)
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
  }

  def fetchOrderHistory(req: GetOrderHistory): Unit = {
    val res: Seq[(String, OrderInfo, Option[Order])] =
      orderHistory.ordersByPairAndAddress(req.assetPair, req.address)
        .map(id => (id, orderHistory.orderInfo(id), orderHistory.order(id))).toSeq.sortBy(_._3.map(_.timestamp).getOrElse(-1L))
    sender() ! GetOrderHistoryResponse(res)
  }

  def fetchAllOrderHistory(req: GetAllOrderHistory): Unit = {
    val res: Seq[(String, OrderInfo, Option[Order])] =
      orderHistory.getAllOrdersByAddress(req.address)
        .map(id => (id, orderHistory.orderInfo(id), orderHistory.order(id))).sortBy(_._3.map(_.timestamp).getOrElse(-1L)).take(settings.restOrderLimit)
    sender() ! GetOrderHistoryResponse(res)
  }

  def getPairTradableBalance(assetPair: AssetPair, address: String): GetTradableBalanceResponse = {
    val bal = (for {
      acc <- Address.fromString(address)
      amountAcc <- Right(AssetAcc(acc, assetPair.amountAsset))
      priceAcc <- Right(AssetAcc(acc, assetPair.priceAsset))
      amountBal <- Right(getTradableBalance(amountAcc))
      priceBal <- Right(getTradableBalance(priceAcc))
    } yield (amountBal, priceBal)) match {
      case Left(_) => (0L, 0L)
      case Right(b) => b
    }

    GetTradableBalanceResponse(Map(
      assetPair.amountAssetStr -> bal._1,
      assetPair.priceAssetStr -> bal._2
    ))
  }

  def getMatcherBalance(addr: String): GetMatcherBalanceResponse = GetMatcherBalanceResponse(orderHistory.openVolumes(addr))

  def deleteFromOrderHistory(req: DeleteOrderFromHistory): Unit = {
    orderHistory.orderStatus(req.id) match {
      case Filled | LimitOrder.Cancelled(_) =>
        orderHistory.deleteOrder(req.assetPair, req.address, req.id)
        sender() ! OrderDeleted(req.id)
      case _ =>
        sender() ! BadMatcherResponse(StatusCodes.BadRequest, "Order couldn't be deleted")
    }
  }

  def recoverFromOrderBook(ob: OrderBook): Unit = {
    ob.asks.foreach{ case (_, orders) =>
      orders.foreach(o => orderHistory.orderAccepted(OrderAdded(o)))
    }
    ob.bids.foreach{ case (_, orders) =>
      orders.foreach(o => orderHistory.orderAccepted(OrderAdded(o)))
    }
  }

}

object OrderHistoryActor {
  val RequestTTL: Int = 5*1000
  val UpdateOpenPortfolioDelay: FiniteDuration = 30 seconds
  def name = "OrderHistory"
  def props(settings: MatcherSettings, utxPool: UtxPool, wallet: Wallet): Props =
    Props(new OrderHistoryActor(settings, utxPool, wallet))

  sealed trait OrderHistoryRequest
  sealed trait ExpirableOrderHistoryRequest extends OrderHistoryRequest {
    def ts: Long
  }
  case class GetOrderHistory(assetPair: AssetPair, address: String, ts: Long) extends ExpirableOrderHistoryRequest
  case class GetAllOrderHistory(address: String, ts: Long) extends ExpirableOrderHistoryRequest
  case class GetOrderStatus(assetPair: AssetPair, id: String, ts: Long) extends ExpirableOrderHistoryRequest
  case class DeleteOrderFromHistory(assetPair: AssetPair, address: String, id: String, ts: Long) extends ExpirableOrderHistoryRequest
  case class ValidateOrder(order: Order, ts: Long) extends ExpirableOrderHistoryRequest
  case class ValidateOrderResult(result: Either[GenericError, Order])
  case class ValidateCancelOrder(cancel: CancelOrder, ts: Long) extends ExpirableOrderHistoryRequest
  case class ValidateCancelResult(result: Either[GenericError, CancelOrder])
  case class RecoverFromOrderBook(ob: OrderBook) extends OrderHistoryRequest

  case class OrderDeleted(orderId: String) extends MatcherResponse {
    val json = Json.obj("status" -> "OrderDeleted", "orderId" -> orderId)
    val code = StatusCodes.OK
  }

  case class GetOrderHistoryResponse(history: Seq[(String, OrderInfo, Option[Order])]) extends MatcherResponse {
    val json = JsArray(history.map(h => Json.obj(
      "id" -> h._1,
      "type" -> h._3.map(_.orderType.toString),
      "amount" -> h._2.amount,
      "price" -> h._3.map(_.price),
      "timestamp" -> h._3.map(_.timestamp),
      "filled" -> h._2.filled,
      "status" -> h._2.status.name,
      "assetPair" -> h._3.map(_.assetPair.json)
    )))
    val code = StatusCodes.OK
  }

  case class GetTradableBalance(assetPair: AssetPair, address: String, ts: Long) extends ExpirableOrderHistoryRequest
  case class GetTradableBalanceResponse(balances: Map[String, Long]) extends MatcherResponse {
    val json: JsObject = JsObject(balances.map{ case (k, v) => (k, JsNumber(v)) })
    val code = StatusCodes.OK
  }

  case class GetMatcherBalance(address: String, ts: Long) extends ExpirableOrderHistoryRequest

  case class GetMatcherBalanceResponse(balances: Option[Map[String, Long]]) extends MatcherResponse {

    //val json = Json.obj("status" -> "OrderCancelRejected", "message" -> message)
    //val code = StatusCodes.BadRequest

    val json: JsObject = JsObject(balances.getOrElse(Map.empty).mapValues(JsNumber(_)))
    val code = balances.map(_ => StatusCodes.OK).getOrElse(StatusCodes.NotFound)
  }
}
