package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Props}
import akka.persistence.PersistentActor
import com.wavesplatform.matcher.market.MatcherActor.OrderBookCreated
import com.wavesplatform.matcher.market.OrderBookActor.GetOrderBookRequest
import com.wavesplatform.matcher.model.LimitOrder
import com.wavesplatform.settings.WavesSettings
import play.api.libs.json.{JsValue, Json}
import scorex.transaction.SimpleTransactionModule._
import scorex.transaction.TransactionModule
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet

object MatcherActor {
  def name = "matcher"
  def props(storedState: StoredState, wallet: Wallet, settings: WavesSettings,
            transactionModule: TransactionModule[StoredInBlock]): Props =
    Props(new MatcherActor(storedState, wallet, settings, transactionModule))

  sealed trait OrderResponse {
    val json: JsValue
    val succeeded: Boolean
  }
  case class OrderAccepted(order: Order) extends OrderResponse {
    val json = order.json
    val succeeded = true
  }
  case class OrderRejected(message: String) extends OrderResponse {
    val json = Json.obj("error" -> "OrderRejected", "message" -> message)
    val succeeded = true
  }
  case object OrderCanceled extends OrderResponse {
    val json = Json.toJson("Order Canceled")
    val succeeded = true
  }

  case class OrderStatus(status: LimitOrder.OrderStatus) extends OrderResponse {
    val json = Json.obj("status" -> "OrderRejected", "message" -> status.toString)
    val succeeded: Boolean = true
  }

  case class OrderBookCreated(pair: AssetPair)
}

class MatcherActor(storedState: StoredState, wallet: Wallet, settings: WavesSettings,
                   transactionModule: TransactionModule[StoredInBlock]
                  ) extends PersistentActor with ScorexLogging {
  def createOrderBook(pair: AssetPair) =
    context.actorOf(OrderBookActor.props(pair, storedState, wallet, settings, transactionModule),
      OrderBookActor.name(pair))

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
