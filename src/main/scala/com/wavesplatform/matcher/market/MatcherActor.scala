package com.wavesplatform.matcher.market

import akka.actor.{Actor, ActorRef, Props}
import akka.persistence.PersistentActor
import com.wavesplatform.matcher.market.MatcherActor.{OrderBookCreated, OrderResponse}
import com.wavesplatform.matcher.market.OrderBookActor.GetOrderBookRequest
import com.wavesplatform.matcher.model.OrderBook
import play.api.libs.json.{JsValue, Json}
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.utils.ScorexLogging

object MatcherActor {
  def name = "matcher"
  def props(orderMatchedActor: ActorRef): Props = Props(new MatcherActor(orderMatchedActor))

  sealed trait OrderResponse {
    val json: JsValue
  }
  case class OrderAccepted(order: Order) extends OrderResponse {
    val json = order.json
  }
  case object OrderCanceled extends OrderResponse {
    val json = Json.toJson("Order Canceled")
  }

  case class OrderBookCreated(pair: AssetPair)
}

class MatcherActor(orderMatchedActor: ActorRef) extends PersistentActor with ScorexLogging {
  def createOrderBook(pair: AssetPair) =
    context.actorOf(OrderBookActor.props(pair, orderMatchedActor), OrderBookActor.name(pair))

  def createAndForward(pair: AssetPair, req: Any) = {
    val orderBook = createOrderBook(pair)
    persistAsync(OrderBookCreated(pair)) { _ =>
      forwardReq(req)(orderBook)
    }
  }

  def forwardReq(req: Any)(orderBook: ActorRef) = orderBook forward req

  def forwardToOrderBook: Receive = {
    case order: Order =>
      context.child(OrderBookActor.name(order.assetPair))
        .fold(createAndForward(order.assetPair, order))(forwardReq(order))
    case ob: GetOrderBookRequest =>
      context.child(OrderBookActor.name(ob.pair))
        .fold(createAndForward(ob.pair, ob))(forwardReq(ob))
  }

  override def receive: Receive = forwardToOrderBook

  override def receiveRecover: Receive = {
    case OrderBookCreated(pair) =>
      context.child(OrderBookActor.name(pair))
        .getOrElse(createOrderBook(pair))
  }

  override def receiveCommand: Receive = forwardToOrderBook

  override def persistenceId: String = "matcher"
}
