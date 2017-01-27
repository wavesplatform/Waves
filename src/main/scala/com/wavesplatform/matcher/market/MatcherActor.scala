package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Props}
import akka.persistence.PersistentActor
import com.wavesplatform.matcher.market.OrderBookActor.{NotFoundPair, OrderBookRequest, OrderRejected}
import com.wavesplatform.matcher.model.Events.OrderBookCreated
import com.wavesplatform.settings.WavesSettings
import scorex.transaction.SimpleTransactionModule._
import scorex.transaction.TransactionModule
import scorex.transaction.assets.exchange.Validation.booleanOperators
import scorex.transaction.assets.exchange.{AssetPair, Order, Validation}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet

import scala.language.reflectiveCalls
import scala.util.Try

object MatcherActor {
  def name = "matcher"
  def props(storedState: StoredState, wallet: Wallet, settings: WavesSettings,
            transactionModule: TransactionModule[StoredInBlock]): Props =
    Props(new MatcherActor(storedState, wallet, settings, transactionModule))
}

class MatcherActor(storedState: StoredState, wallet: Wallet, settings: WavesSettings,
                   transactionModule: TransactionModule[StoredInBlock]
                  ) extends PersistentActor with ScorexLogging {

  def createOrderBook(pair: AssetPair): ActorRef =
    context.actorOf(OrderBookActor.props(pair, storedState, wallet, settings, transactionModule),
      OrderBookActor.name(pair))

  def basicValidation(msg: {def assetPair: AssetPair}): Validation = {
    Try(msg.assetPair).isSuccess :| "Invalid AssetPair" &&
      msg.assetPair.first.map(storedState.assetsExtension.getAssetQuantity).forall(_ > 0) :| "Not enough balance" &&
      msg.assetPair.second.map(storedState.assetsExtension.getAssetQuantity).forall(_ > 0) :| "Not enough balance"
  }

  def createAndForward(order: Order): Unit = {
    val orderBook = createOrderBook(order.assetPair)
    persistAsync(OrderBookCreated(order.assetPair)) { _ =>
      forwardReq(order)(orderBook)
    }
  }

  def returNotFound(): Unit = {
    sender() ! NotFoundPair
  }

  def forwardReq(req: Any)(orderBook: ActorRef): Unit = orderBook forward req

  def checkAssetPair[A <: { def assetPair: AssetPair }](msg: A)(f: => Unit): Unit = {
    val v = basicValidation(msg)
    if (v) f
    else sender() ! OrderRejected(v.messages())
  }

  def forwardToOrderBook: Receive = {
    case order: Order =>
      checkAssetPair(order) {
        context.child(OrderBookActor.name(order.assetPair))
          .fold(createAndForward(order))(forwardReq(order))
      }
    case ob: OrderBookRequest =>
      checkAssetPair(ob) {
        context.child(OrderBookActor.name(ob.assetPair))
          .fold(returNotFound())(forwardReq(ob))
      }
  }

  override def receiveRecover: Receive = {
    case OrderBookCreated(pair) =>
      context.child(OrderBookActor.name(pair))
        .getOrElse(createOrderBook(pair))
  }

  override def receiveCommand: Receive = forwardToOrderBook

  override def persistenceId: String = "matcher"
}
