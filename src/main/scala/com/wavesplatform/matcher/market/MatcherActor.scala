package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Props}
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.persistence.PersistentActor
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.{MatcherResponse, StatusCodeMatcherResponse}
import com.wavesplatform.matcher.market.OrderBookActor.{GetOrderBookResponse, OrderBookRequest}
import play.api.libs.json.{JsArray, JsValue, Json}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.Validation.booleanOperators
import scorex.transaction.assets.exchange.{AssetPair, Order, Validation}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{AssetId, TransactionModule}
import scorex.utils.{NTP, ScorexLogging}
import scorex.wallet.Wallet

import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.util.Try

class MatcherActor(storedState: StoredState, wallet: Wallet, settings: MatcherSettings,
                   transactionModule: TransactionModule
                  ) extends PersistentActor with ScorexLogging {

  import MatcherActor._

  val openMarkets: mutable.Buffer[MarketData] = mutable.Buffer.empty[MarketData]

  def createOrderBook(pair: AssetPair): ActorRef = {
    def getAssetName(asset: Option[AssetId]) = asset.map(storedState.assetsExtension.getAssetName).getOrElse(AssetPair.WavesName)

    openMarkets += MarketData(pair, getAssetName(pair.first), getAssetName(pair.second), NTP.correctedTime())

    context.actorOf(OrderBookActor.props(pair, storedState, wallet, settings, transactionModule),
      OrderBookActor.name(pair))
  }

  def basicValidation(msg: {def assetPair: AssetPair}): Validation = {
    Try(msg.assetPair).isSuccess :| "Invalid AssetPair" &&
      msg.assetPair.first.map(storedState.assetsExtension.getAssetQuantity).forall(_ > 0) :|
        s"Unknown Asset ID: ${msg.assetPair.firstStr}" &&
      msg.assetPair.second.map(storedState.assetsExtension.getAssetQuantity).forall(_ > 0) :|
        s"Unknown Asset ID: ${msg.assetPair.secondStr}"
  }

  def createAndForward(order: Order): Unit = {
    val orderBook = createOrderBook(order.assetPair)
    persistAsync(OrderBookCreated(order.assetPair)) { _ =>
      forwardReq(order)(orderBook)
    }
  }

  def returnEmptyOrderBook(pair: AssetPair): Unit = {
    sender() ! GetOrderBookResponse(pair, Seq(), Seq())
  }

  def forwardReq(req: Any)(orderBook: ActorRef): Unit = orderBook forward req

  def checkAssetPair[A <: {def assetPair : AssetPair}](msg: A)(f: => Unit): Unit = {
    val v = basicValidation(msg)
    if (v) f
    else sender() ! StatusCodeMatcherResponse(StatusCodes.NotFound, v.messages())
  }

  def getMatcherPublicKey: Array[Byte] = {
    wallet.privateKeyAccount(settings.account).map(_.publicKey).getOrElse(Array())
  }

  def forwardToOrderBook: Receive = {
    case GetMarkets =>
      sender() ! GetMarketsResponse(getMatcherPublicKey, openMarkets)
    case order: Order =>
      checkAssetPair(order) {
        context.child(OrderBookActor.name(order.assetPair))
          .fold(createAndForward(order))(forwardReq(order))
      }
    case ob: OrderBookRequest =>
      checkAssetPair(ob) {
        context.child(OrderBookActor.name(ob.assetPair))
          .fold(returnEmptyOrderBook(ob.assetPair))(forwardReq(ob))
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

  def props(storedState: StoredState, wallet: Wallet, settings: MatcherSettings,
            transactionModule: TransactionModule): Props =
    Props(new MatcherActor(storedState, wallet, settings, transactionModule))

  case class OrderBookCreated(pair: AssetPair)

  case object GetMarkets

  case class GetMarketsResponse(publicKey: Array[Byte], markets: Seq[MarketData]) extends MatcherResponse {
    def getMarketsJs: JsValue = JsArray(markets.map(m => Json.obj(
      "asset1Id" -> m.pair.firstStr,
      "asset1Name" -> m.firstAssetName,
      "asset2Id" -> m.pair.secondStr,
      "asset2Name" -> m.secondAssetName,
      "created" -> m.created
    ))
    )

    def json: JsValue = Json.obj(
      "matcherPublicKey" -> Base58.encode(publicKey),
      "markets" -> getMarketsJs
    )

    def code: StatusCode = StatusCodes.OK
  }

  case class MarketData(pair: AssetPair, firstAssetName: String, secondAssetName: String, created: Long)

}
