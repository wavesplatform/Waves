package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Props}
import akka.persistence.PersistentActor
import com.wavesplatform.matcher.market.MatcherActor.OrderBookCreated
import com.wavesplatform.matcher.market.OrderBookActor.{NotFoundPair, OrderBookRequest, OrderRejected}
import com.wavesplatform.settings.WavesSettings
import scorex.transaction.SimpleTransactionModule._
import scorex.transaction.{AssetAcc, TransactionModule}
import scorex.transaction.assets.exchange.{AssetPair, Order, Validation}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.{NTP, ScorexLogging}
import scorex.wallet.Wallet
import scorex.transaction.assets.exchange.Validation.BooleanOperators

object MatcherActor {
  def name = "matcher"
  def props(storedState: StoredState, wallet: Wallet, settings: WavesSettings,
            transactionModule: TransactionModule[StoredInBlock]): Props =
    Props(new MatcherActor(storedState, wallet, settings, transactionModule))

  case class OrderBookCreated(pair: AssetPair)
}

class MatcherActor(storedState: StoredState, wallet: Wallet, settings: WavesSettings,
                   transactionModule: TransactionModule[StoredInBlock]
                  ) extends PersistentActor with ScorexLogging {
  def createOrderBook(pair: AssetPair) =
    context.actorOf(OrderBookActor.props(pair, storedState, wallet, settings, transactionModule),
      OrderBookActor.name(pair))

  def basicValidation(order: Order): Validation = {
      order.isValid(NTP.correctedTime()) &&
        (storedState.assetBalance(AssetAcc(order.sender, order.spendAssetId)) >= order.getSpendAmount()) :| "Not enough balance"
  }

  def createAndForward(order: Order) = {
    val v = basicValidation(order)
    if (v) {
      val orderBook = createOrderBook(order.assetPair)
      persistAsync(OrderBookCreated(order.assetPair)) { _ =>
        forwardReq(order)(orderBook)
      }
    } else sender() ! OrderRejected(v.messages)
  }

  def returNotFound() = {
    sender() ! NotFoundPair
  }

  def forwardReq(req: Any)(orderBook: ActorRef) = orderBook forward req

  def forwardToOrderBook: Receive = {
    case order: Order =>
      context.child(OrderBookActor.name(order.assetPair))
        .fold(createAndForward(order))(forwardReq(order))
    case ob: OrderBookRequest =>
      context.child(OrderBookActor.name(ob.pair))
        .fold(returNotFound())(forwardReq(ob))
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
