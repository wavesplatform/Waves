package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Props}
import akka.persistence.PersistentActor
import com.wavesplatform.matcher.api.{GenericMatcherResponse, MatcherResponse}
import com.wavesplatform.matcher.market.OrderBookActor.OrderBookRequest
import com.wavesplatform.settings.WavesSettings
import play.api.libs.json.{JsArray, JsNull, JsValue, Json}
import scorex.crypto.encode.Base58
import scorex.transaction.SimpleTransactionModule._
import scorex.transaction.assets.exchange.Validation.booleanOperators
import scorex.transaction.assets.exchange.{AssetPair, Order, Validation}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{AssetId, TransactionModule}
import scorex.utils.{NTP, ScorexLogging}
import scorex.wallet.Wallet

import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.util.Try

class MatcherActor(storedState: StoredState, wallet: Wallet, settings: WavesSettings,
                   transactionModule: TransactionModule
                  ) extends PersistentActor with ScorexLogging {
  import MatcherActor._

  val openMarkets: mutable.Buffer[MarketData] = mutable.Buffer.empty[MarketData]

  def createOrderBook(pair: AssetPair): ActorRef = {
    def getAssetName(asset: Option[AssetId]) = asset.map(storedState.assetsExtension.getAssetName).getOrElse("WAVES")

    openMarkets += MarketData(pair, getAssetName(pair.first), getAssetName(pair.second), NTP.correctedTime())

    context.actorOf(OrderBookActor.props(pair, storedState, wallet, settings, transactionModule),
      OrderBookActor.name(pair))
  }

  def basicValidation(msg: {def assetPair: AssetPair}): Validation = {
    Try(msg.assetPair).isSuccess :| "Invalid AssetPair" &&
      msg.assetPair.first.map(storedState.assetsExtension.getAssetQuantity).forall(_ > 0) :| "Unknown Asset ID" &&
      msg.assetPair.second.map(storedState.assetsExtension.getAssetQuantity).forall(_ > 0) :| "Unknown Asset ID"
  }

  def createAndForward(order: Order): Unit = {
    val orderBook = createOrderBook(order.assetPair)
    persistAsync(OrderBookCreated(order.assetPair)) { _ =>
      forwardReq(order)(orderBook)
    }
  }

  def returnNotFoundPair(): Unit = {
    sender() ! GetTradableResponse(false, "Unknown Assets Pair")
  }

  def forwardReq(req: Any)(orderBook: ActorRef): Unit = orderBook forward req

  def checkAssetPair[A <: { def assetPair: AssetPair }](msg: A)(f: => Unit): Unit = {
    val v = basicValidation(msg)
    if (v) f
    else sender() ! GetTradableResponse(v.status, v.messages())
  }

  def forwardToOrderBook: Receive = {
    case m: GetMarkets =>
      sender() ! GetMarketsResponse(openMarkets)
    case GetTradable(a1, a2) =>
      val msg = new {def assetPair = AssetPair(a1, a2)}
      checkAssetPair(msg) (sender() ! GetTradableResponse(true, s"Pair: ${msg.assetPair} is tradable"))
    case order: Order =>
      checkAssetPair(order) {
        context.child(OrderBookActor.name(order.assetPair))
          .fold(createAndForward(order))(forwardReq(order))
      }
    case ob: OrderBookRequest =>
      checkAssetPair(ob) {
        context.child(OrderBookActor.name(ob.assetPair))
          .fold(returnNotFoundPair())(forwardReq(ob))
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

object MatcherActor {
  def name = "matcher"
  def props(storedState: StoredState, wallet: Wallet, settings: WavesSettings,
            transactionModule: TransactionModule): Props =
    Props(new MatcherActor(storedState, wallet, settings, transactionModule))

  case class OrderBookCreated(pair: AssetPair)

  case class GetMarkets()

  case class GetMarketsResponse(markets: Seq[MarketData]) extends GenericMatcherResponse {
    val success = true
    val message = ""
    override val result: JsValue = JsArray(markets.map(m => Json.obj(
        "firstAssetId" -> m.pair.first.map(Base58.encode),
        "firstAssetName" -> m.firstAssetName,
        "secondAssetId" -> m.pair.second.map(Base58.encode),
        "secondAssetName" -> m.secondAssetName,
        "created" -> m.created
      ))
    )
  }

  case class GetTradable(asset1: Option[AssetId], asset2: Option[AssetId])
  case class GetTradableResponse(success: Boolean, message: String) extends GenericMatcherResponse

  case class MarketData(pair: AssetPair, firstAssetName: String, secondAssetName: String, created: Long)
}