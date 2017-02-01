package com.wavesplatform.matcher.market

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import akka.actor.{ActorRef, Props}
import akka.persistence.PersistentActor
import com.wavesplatform.matcher.market.OrderBookActor.{NotFoundPair, OrderBookRequest, OrderRejected}
import com.wavesplatform.settings.WavesSettings
import play.api.libs.json.{JsArray, JsValue, Json}
import scorex.crypto.encode.Base58
import scorex.transaction.SimpleTransactionModule._
import scorex.transaction.assets.exchange.Validation.booleanOperators
import scorex.transaction.assets.exchange.{AssetPair, Order, Validation}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{AssetId, TransactionModule}
import scorex.utils.ScorexLogging
import scorex.wallet.Wallet

import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.util.Try

object MatcherActor {
  def name = "matcher"
  def props(storedState: StoredState, wallet: Wallet, settings: WavesSettings,
            transactionModule: TransactionModule): Props =
    Props(new MatcherActor(storedState, wallet, settings, transactionModule))

  case class OrderBookCreated(pair: AssetPair)
  case class GetMarkets()
  case class GetMarketsResponse(markets: Seq[MarketData]) {
    def json: JsValue = Json.obj(
      "result" -> JsArray(markets.map(m => Json.obj(
        "firstAssetId" -> m.pair.first.map(Base58.encode),
        "firstAssetName" -> m.firstAssetName,
        "secondAssetId" -> m.pair.second.map(Base58.encode),
        "secondAssetName" -> m.secondAssetName,
        "created" -> m.created.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
      )))
    )
  }

  case class MarketData(pair: AssetPair, firstAssetName: String, secondAssetName: String, created: LocalDateTime)
}

class MatcherActor(storedState: StoredState, wallet: Wallet, settings: WavesSettings,
                   transactionModule: TransactionModule
                  ) extends PersistentActor with ScorexLogging {
  import MatcherActor._

  val openMarkets: mutable.Buffer[MarketData] = mutable.Buffer.empty[MarketData]

  def createOrderBook(pair: AssetPair): ActorRef = {
    def getAssetName(asset: Option[AssetId]) = asset.map(storedState.assetsExtension.getAssetName).getOrElse("WAVES")

    openMarkets += MarketData(pair, getAssetName(pair.first), getAssetName(pair.second), LocalDateTime.now().withNano(0))

    context.actorOf(OrderBookActor.props(pair, storedState, wallet, settings, transactionModule),
      OrderBookActor.name(pair))
  }

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

  def returnNotFound(): Unit = {
    sender() ! NotFoundPair
  }

  def forwardReq(req: Any)(orderBook: ActorRef): Unit = orderBook forward req

  def checkAssetPair[A <: { def assetPair: AssetPair }](msg: A)(f: => Unit): Unit = {
    val v = basicValidation(msg)
    if (v) f
    else sender() ! OrderRejected(v.messages())
  }

  def forwardToOrderBook: Receive = {
    case GetMarkets =>
      sender() ! GetMarketsResponse(openMarkets)
    case order: Order =>
      checkAssetPair(order) {
        context.child(OrderBookActor.name(order.assetPair))
          .fold(createAndForward(order))(forwardReq(order))
      }
    case ob: OrderBookRequest =>
      checkAssetPair(ob) {
        context.child(OrderBookActor.name(ob.assetPair))
          .fold(returnNotFound())(forwardReq(ob))
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
