package com.wavesplatform.matcher.market

import java.io.File

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.{BadMatcherResponse, StatusCodeMatcherResponse}
import com.wavesplatform.matcher.market.OrderBookActor.{CancelOrder, GetOrderHistoryResponse, GetOrderStatusResponse}
import com.wavesplatform.matcher.market.OrderHistoryActor._
import com.wavesplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.wavesplatform.matcher.model.LimitOrder.Filled
import com.wavesplatform.matcher.model._
import com.wavesplatform.state2.reader.StateReader
import org.h2.mvstore.MVStore
import scorex.transaction.ValidationError.CustomError
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.wallet.Wallet

class OrderHistoryActor(val settings: MatcherSettings, val storedState: StateReader, val wallet: Wallet)
  extends Actor with OrderValidator {

  val dbFile = new File(settings.orderHistoryFile)
  dbFile.getParentFile.mkdirs().ensuring(dbFile.getParentFile.exists())
  val db: MVStore = new MVStore.Builder().fileName(settings.orderHistoryFile).compress().open()
  val storage = new OrderHistoryStorage(db)
  val orderHistory = OrderHistoryImpl(storage)

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[OrderAdded])
    context.system.eventStream.subscribe(self, classOf[OrderExecuted])
    context.system.eventStream.subscribe(self, classOf[OrderCanceled])
  }

  override def receive: Receive = {
    case ev: OrderAdded =>
      orderHistory.didOrderAccepted(ev)
    case ev: OrderExecuted =>
      orderHistory.didOrderExecuted(ev)
    case ev: OrderCanceled =>
      orderHistory.didOrderCanceled(ev)
    case req: GetOrderHistory =>
      fetchOrderHistory(req)
    case ValidateOrder(o) =>
      sender() ! ValidateOrderResult(validateNewOrder(o))
    case ValidateCancelOrder(co) =>
      sender() ! ValidateCancelResult(validateCancelOrder(co))
    case req: DeleteOrderFromHistory =>
      deleteFromOrderHistory(req)
    case GetOrderStatus(_, id) =>
      sender() ! GetOrderStatusResponse(orderHistory.getOrderStatus(id))
    case RecoverFromOrderBook(ob) =>
      recoverFromOrderBook(ob)
  }

  def fetchOrderHistory(req: GetOrderHistory): Unit = {
    val res: Seq[(String, OrderInfo, Option[Order])] =
      orderHistory.getOrdersByPairAndAddress(req.assetPair, req.address)
        .map(id => (id, orderHistory.getOrderInfo(id), orderHistory.getOrder(id))).toSeq.sortBy(_._3.map(_.timestamp).getOrElse(-1L))
    sender() ! GetOrderHistoryResponse(res)
  }

  def validateOrder(): Unit = {

  }

  def deleteFromOrderHistory(req: DeleteOrderFromHistory): Unit = {
    orderHistory.getOrderStatus(req.id) match {
      case Filled | LimitOrder.Cancelled(_) =>
        orderHistory.deleteOrder(req.assetPair, req.address, req.id)
        sender() ! StatusCodeMatcherResponse(StatusCodes.OK, "Order deleted")
      case _ =>
        sender() ! BadMatcherResponse(StatusCodes.BadRequest, "Order couldn't be deleted")
    }
  }

  def recoverFromOrderBook(ob: OrderBook): Unit = {
    ob.asks.foreach{ case (_, orders) =>
      orders.foreach(o => orderHistory.didOrderAccepted(OrderAdded(o)))
    }
    ob.bids.foreach{ case (_, orders) =>
      orders.foreach(o => orderHistory.didOrderAccepted(OrderAdded(o)))
    }
  }

}

object OrderHistoryActor {
  def name = "OrderHistory"
  def props(settings: MatcherSettings, storedState: StateReader, wallet: Wallet): Props =
    Props(new OrderHistoryActor(settings, storedState, wallet))

  sealed trait OrderHistoryRequest
  case class GetOrderHistory(assetPair: AssetPair, address: String) extends OrderHistoryRequest
  case class GetOrderStatus(assetPair: AssetPair, id: String) extends OrderHistoryRequest
  case class DeleteOrderFromHistory(assetPair: AssetPair, address: String, id: String) extends OrderHistoryRequest
  case class ValidateOrder(order: Order) extends OrderHistoryRequest
  case class ValidateOrderResult(result: Either[CustomError, Order])
  case class ValidateCancelOrder(cancel: CancelOrder) extends OrderHistoryRequest
  case class ValidateCancelResult(result: Either[CustomError, CancelOrder])
  case class RecoverFromOrderBook(ob: OrderBook) extends OrderHistoryRequest
}