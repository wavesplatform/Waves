package com.wavesplatform.matcher.market

import akka.actor.{ActorRef, Props}
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.persistence.{PersistentActor, RecoveryCompleted}
import com.google.common.base.Charsets
import com.wavesplatform.UtxPool
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.{MatcherResponse, StatusCodeMatcherResponse}
import com.wavesplatform.matcher.market.OrderBookActor.{DeleteOrderBookRequest, GetOrderBookResponse, OrderBookRequest}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.SnapshotStateReader
import io.netty.channel.group.ChannelGroup
import play.api.libs.json._
import scorex.account.Address
import scorex.crypto.encode.Base58
import scorex.transaction.assets.exchange.Validation.booleanOperators
import scorex.transaction.assets.exchange.{AssetPair, Order, Validation}
import scorex.transaction.{AssetId, History}
import scorex.utils._
import scorex.wallet.Wallet

import scala.collection.{immutable, mutable}
import scala.language.reflectiveCalls

class MatcherActor(orderHistory: ActorRef, storedState: SnapshotStateReader, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup,
                   settings: MatcherSettings, history: History, functionalitySettings: FunctionalitySettings)
  extends PersistentActor with ScorexLogging {

  import MatcherActor._

  val tradedPairs = mutable.Map.empty[AssetPair, MarketData]

  def getAssetName(asset: Option[AssetId]): String =
    asset.fold(AssetPair.WavesName) { aid =>
      // fixme: the following line will throw an exception when asset name bytes are not a valid UTF-8
      storedState.assetDescription(aid).fold("Unknown")(d => new String(d.name, Charsets.UTF_8))
    }

  def createOrderBook(pair: AssetPair): ActorRef = {
    val md = MarketData(pair, getAssetName(pair.amountAsset), getAssetName(pair.priceAsset), NTP.correctedTime(),
      pair.amountAsset.flatMap(storedState.assetDescription).map(t => AssetInfo(t.decimals)),
      pair.priceAsset.flatMap(storedState.assetDescription).map(t => AssetInfo(t.decimals)))
    tradedPairs += pair -> md

    context.actorOf(OrderBookActor.props(pair, orderHistory, storedState, settings, wallet, utx, allChannels, history, functionalitySettings),
      OrderBookActor.name(pair))
  }

  def basicValidation(msg: {def assetPair: AssetPair}): Validation = {
    val s = storedState
    def isAssetsExist: Validation = {
      msg.assetPair.priceAsset.forall(s.assetDescription(_).isDefined) :|
        s"Unknown Asset ID: ${msg.assetPair.priceAssetStr}" &&
        msg.assetPair.amountAsset.forall(s.assetDescription(_).isDefined) :|
          s"Unknown Asset ID: ${msg.assetPair.amountAssetStr}"
    }

    msg.assetPair.isValid :| "Invalid AssetPair" && isAssetsExist
  }

  def checkPairOrdering(aPair: AssetPair): Validation = {
    val reversePair = AssetPair(aPair.priceAsset, aPair.amountAsset)

    val isCorrectOrder = if (tradedPairs.contains(aPair)) true
    else if (tradedPairs.contains(reversePair)) false
    else if (settings.priceAssets.contains(aPair.priceAssetStr) && settings.priceAssets.contains(aPair.amountAssetStr)) {
      settings.priceAssets.indexOf(aPair.priceAssetStr) < settings.priceAssets.indexOf(aPair.amountAssetStr)
    }
    else if (settings.priceAssets.contains(aPair.priceAssetStr)) true
    else if (settings.priceAssets.contains(reversePair.priceAssetStr)) false
    else compare(aPair.priceAsset.map(_.arr), aPair.amountAsset.map(_.arr)) < 0

    isCorrectOrder :| s"Invalid AssetPair ordering, should be reversed: $reversePair"
  }

  def checkBlacklistRegex(aPair: AssetPair): Validation = {
      val (amountName, priceName) = (getAssetName(aPair.amountAsset), getAssetName(aPair.priceAsset))
      settings.blacklistedNames.forall(_.findFirstIn(amountName).isEmpty) :| s"Invalid Asset Name: $amountName" &&
        settings.blacklistedNames.forall(_.findFirstIn(priceName).isEmpty) :| s"Invalid Asset Name: $priceName"
  }

  def checkBlacklistId(aPair: AssetPair): Validation = {
    !settings.blacklistedAssets.contains(aPair.priceAssetStr) :| s"Invalid Asset ID: ${aPair.priceAssetStr}" &&
      !settings.blacklistedAssets.contains(aPair.amountAssetStr) :| s"Invalid Asset ID: ${aPair.amountAssetStr}"
  }

  def checkBlacklistedAddress(address: Address)(f: => Unit): Unit = {
    val v =  !settings.blacklistedAddresses.contains(address.address) :| s"Invalid Address: ${address.address}"
    if (!v) {
      sender() ! StatusCodeMatcherResponse(StatusCodes.Forbidden, v.messages())
    } else {
      f
    }
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
    val v =  checkBlacklistId(msg.assetPair) && basicValidation(msg) && checkBlacklistRegex(msg.assetPair)
    if (!v) {
      sender() ! StatusCodeMatcherResponse(StatusCodes.NotFound, v.messages())
    } else {
      val ov = checkPairOrdering(msg.assetPair)
      if (!ov) {
        sender() ! StatusCodeMatcherResponse(StatusCodes.Found, ov.messages())
      } else {
        f
      }
    }
  }

  def getMatcherPublicKey: Array[Byte] = {
    wallet.findWallet(settings.account).map(_.publicKey).getOrElse(Array())
  }

  def forwardToOrderBook: Receive = {
    case GetMarkets =>
      sender() ! GetMarketsResponse(getMatcherPublicKey, tradedPairs.values.toSeq)
    case order: Order =>
      checkAssetPair(order) {
        checkBlacklistedAddress(order.senderPublicKey) {
          context.child(OrderBookActor.name(order.assetPair))
            .fold(createAndForward(order))(forwardReq(order))
        }
      }
    case ob: DeleteOrderBookRequest =>
      checkAssetPair(ob) {
        context.child(OrderBookActor.name(ob.assetPair))
          .fold(returnEmptyOrderBook(ob.assetPair))(forwardReq(ob))
        removeOrderBook(ob.assetPair)
      }
    case ob: OrderBookRequest =>
      checkAssetPair(ob) {
        context.child(OrderBookActor.name(ob.assetPair))
          .fold(returnEmptyOrderBook(ob.assetPair))(forwardReq(ob))
      }
  }

  def initPredefinedPairs(): Unit = {
    settings.predefinedPairs.diff(tradedPairs.keys.toSeq).foreach(pair =>
      createOrderBook(pair)
    )
  }

  private def removeOrderBook(pair: AssetPair): Unit = {
    if (tradedPairs.contains(pair)) {
      tradedPairs -= pair
      deleteMessages(lastSequenceNr)
      persistAll(tradedPairs.map(v => OrderBookCreated(v._1)).to[immutable.Seq]) { _ => }
    }
  }

  override def receiveRecover: Receive = {
    case OrderBookCreated(pair) =>
      context.child(OrderBookActor.name(pair))
        .getOrElse(createOrderBook(pair))
    case RecoveryCompleted =>
      log.info("MatcherActor - Recovery completed!")
      initPredefinedPairs()
  }

  override def receiveCommand: Receive = forwardToOrderBook

  override def persistenceId: String = "matcher"
}

object MatcherActor {
  def name = "matcher"

  def props(orderHistoryActor: ActorRef, storedState: SnapshotStateReader, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup,
            settings: MatcherSettings, history: History, functionalitySettings: FunctionalitySettings): Props =
    Props(new MatcherActor(orderHistoryActor, storedState, wallet, utx, allChannels,settings, history, functionalitySettings))

  case class OrderBookCreated(pair: AssetPair)

  case object GetMarkets

  case class GetMarketsResponse(publicKey: Array[Byte], markets: Seq[MarketData]) extends MatcherResponse {
    def getMarketsJs: JsValue = JsArray(markets.map(m => Json.obj(
      "amountAsset" -> m.pair.amountAssetStr,
      "amountAssetName" -> m.amountAssetName,
      "amountAssetInfo" -> m.amountAssetInfo,
      "priceAsset" -> m.pair.priceAssetStr,
      "priceAssetName" -> m.priceAssetName,
      "priceAssetInfo" -> m.priceAssetinfo,
      "created" -> m.created
    ))
    )

    def json: JsValue = Json.obj(
      "matcherPublicKey" -> Base58.encode(publicKey),
      "markets" -> getMarketsJs
    )

    def code: StatusCode = StatusCodes.OK
  }

  case class AssetInfo(decimals: Int)
  implicit val assetInfoFormat: Format[AssetInfo] = Json.format[AssetInfo]

  case class MarketData(pair: AssetPair, amountAssetName: String, priceAssetName: String, created: Long,
                        amountAssetInfo: Option[AssetInfo], priceAssetinfo: Option[AssetInfo])

  def compare(buffer1: Option[Array[Byte]], buffer2: Option[Array[Byte]]): Int = {
    if (buffer1.isEmpty && buffer2.isEmpty) 0
    else if (buffer1.isEmpty) -1
    else if (buffer2.isEmpty) 1
    else ByteArray.compare(buffer1.get, buffer2.get)
  }
}
