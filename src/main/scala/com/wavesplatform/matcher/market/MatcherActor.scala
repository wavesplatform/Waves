package com.wavesplatform.matcher.market

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorRef, Props}
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import akka.persistence._
import akka.routing.FromConfig
import com.google.common.base.Charsets
import com.wavesplatform.matcher.api.{BadMatcherResponse, MatcherResponse, StatusCodeMatcherResponse}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.BalanceChanged
import com.wavesplatform.matcher.model.OrderBook
import com.wavesplatform.matcher.{AssetPairBuilder, MatcherSettings}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.{AssetDescription, Blockchain}
import com.wavesplatform.utils.Base58
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import play.api.libs.json._
import scorex.account.Address
import scorex.transaction.AssetId
import scorex.transaction.assets.exchange.Validation.booleanOperators
import scorex.transaction.assets.exchange.{AssetPair, Order}
import scorex.utils._
import scorex.wallet.Wallet

import scala.collection.immutable

class MatcherActor(orderHistory: ActorRef,
                   pairBuilder: AssetPairBuilder,
                   orderBooks: AtomicReference[Map[AssetPair, ActorRef]],
                   updateSnapshot: AssetPair => OrderBook => Unit,
                   wallet: Wallet,
                   utx: UtxPool,
                   allChannels: ChannelGroup,
                   settings: MatcherSettings,
                   blockchain: Blockchain,
                   functionalitySettings: FunctionalitySettings)
    extends PersistentActor
    with ScorexLogging {

  import MatcherActor._

  private var shutdownInitiated      = false
  private var tradedPairs            = Map.empty[AssetPair, MarketData]
  private var lastSnapshotSequenceNr = 0L

  private def orderBook(pair: AssetPair) = Option(orderBooks.get()).flatMap(_.get(pair))

  def getAssetName(asset: Option[AssetId], desc: Option[AssetDescription]): String =
    asset.fold(AssetPair.WavesName) { _ =>
      desc.fold("Unknown")(d => new String(d.name, Charsets.UTF_8))
    }

  private def createMarketData(pair: AssetPair): MarketData = {
    val amountDesc = pair.amountAsset.flatMap(blockchain.assetDescription)
    val priceDesc  = pair.amountAsset.flatMap(blockchain.assetDescription)

    MarketData(
      pair,
      getAssetName(pair.amountAsset, amountDesc),
      getAssetName(pair.priceAsset, priceDesc),
      NTP.correctedTime(),
      amountDesc.map(t => AssetInfo(t.decimals)),
      priceDesc.map(t => AssetInfo(t.decimals))
    )
  }

  private def createOrderBookActor(pair: AssetPair): ActorRef = context.actorOf(
    OrderBookActor.props(pair, updateSnapshot(pair), orderHistory, blockchain, settings, wallet, utx, allChannels, functionalitySettings),
    OrderBookActor.name(pair)
  )

  def createOrderBook(pair: AssetPair): ActorRef = {
    tradedPairs += pair -> createMarketData(pair)
    val orderBook = createOrderBookActor(pair)
    orderBooks.updateAndGet(_ + (pair -> orderBook))
    orderBook
  }

  def checkBlacklistedAddress(address: Address)(f: => Unit): Unit = {
    val v = !settings.blacklistedAddresses.contains(address.address) :| s"Invalid Address: ${address.address}"
    if (v) f else sender() ! StatusCodeMatcherResponse(StatusCodes.Forbidden, v.messages())
  }

  def createAndForward(order: Order): Unit = {
    val orderBook = createOrderBook(order.assetPair)
    persistAsync(OrderBookCreated(order.assetPair)) { _ =>
      forwardReq(order)(orderBook)
    }
  }

  def returnEmptyOrderBook(pair: AssetPair): Unit = {
    sender() ! GetOrderBookResponse.empty(pair)
  }

  def forwardReq(req: Any)(orderBook: ActorRef): Unit = orderBook forward req

  def checkAssetPair(assetPair: AssetPair, msg: Any)(f: => Unit): Unit =
    pairBuilder.validateAssetPair(assetPair) match {
      case Right(_) => f
      case Left(e) =>
        sender() ! pairBuilder
          .validateAssetPair(assetPair.reverse)
          .fold(
            _ => StatusCodeMatcherResponse(StatusCodes.NotFound, e),
            _ => StatusCodeMatcherResponse(StatusCodes.Found, e)
          )
    }

  def getMatcherPublicKey: Array[Byte] = {
    wallet.findPrivateKey(settings.account).map(_.publicKey).getOrElse(Array())
  }

  def forwardToOrderBook: Receive = {
    case GetMarkets =>
      sender() ! GetMarketsResponse(getMatcherPublicKey, tradedPairs.values.toSeq)

    case order: Order =>
      checkAssetPair(order.assetPair, order) {
        checkBlacklistedAddress(order.senderPublicKey) {
          orderBook(order.assetPair).fold(createAndForward(order))(forwardReq(order))
        }
      }

    case ob: DeleteOrderBookRequest =>
      checkAssetPair(ob.assetPair, ob) {
        orderBook(ob.assetPair)
          .fold(returnEmptyOrderBook(ob.assetPair))(forwardReq(ob))
        removeOrderBook(ob.assetPair)
      }

    case x: ForceCancelOrder =>
      checkAssetPair(x.assetPair, x) {
        orderBook(x.assetPair)
          .fold {
            sender() ! OrderCancelRejected(s"Order '${x.orderId}' is already cancelled or never existed in '${x.assetPair.key}' pair")
          }(forwardReq(x))
      }

    case Shutdown =>
      if (lastSnapshotSequenceNr < lastSequenceNr) saveSnapshot(Snapshot(tradedPairs.keySet))
      context.actorOf(Props(classOf[GracefulShutdownActor], context.children.toVector, self))
  }

  def shuttingDown: Receive = {
    case ShutdownComplete =>
      log.info("OrderBooks are successfully closed, stopping MatcherActor")
      context.stop(self)
    case _ => sender() ! BadMatcherResponse(StatusCodes.ServiceUnavailable, "System is going shutdown")
  }

  def initPredefinedPairs(): Unit = {
    settings.predefinedPairs.filterNot(tradedPairs.contains).foreach(createOrderBook)
  }

  private def removeOrderBook(pair: AssetPair): Unit = {
    if (tradedPairs.contains(pair)) {
      tradedPairs -= pair
      deleteMessages(lastSequenceNr)
      persistAll(tradedPairs.map(v => OrderBookCreated(v._1)).to[immutable.Seq])(_ => ())
    }
  }

  override def receiveRecover: Receive = {
    case OrderBookCreated(pair) =>
      log.info(s"==> OrderBookCreated($pair)")
      if (orderBook(pair).isEmpty) createOrderBook(pair)

    case SnapshotOffer(_, snapshot: Snapshot) =>
      lastSnapshotSequenceNr = lastSequenceNr

//      var orderBooksCache = List.empty[(AssetPair, ActorRef)]
//      snapshot.tradedPairsSet.par.foreach { pair =>
//        tradedPairs += pair      -> createMarketData(pair)
//        orderBooksCache ::= pair -> createOrderBookActor(pair)
//      }
//      orderBooks.updateAndGet(_ ++ orderBooksCache)
      snapshot.tradedPairsSet.par.foreach(createOrderBook)

    case RecoveryCompleted =>
      log.info("MatcherActor - Recovery completed!")
      initPredefinedPairs()
      createBalanceWatcher()
  }

  private def snapshotsCommands: Receive = {
    case SaveSnapshotSuccess(metadata) =>
      deleteMessages(metadata.sequenceNr - 1)
      log.info(s"Snapshot saved with metadata $metadata")

    case SaveSnapshotFailure(metadata, reason) =>
      log.error(s"Failed to save snapshot: $metadata, $reason.")
  }

  override def receiveCommand: Receive = /*shutdownFallback orElse */ forwardToOrderBook orElse snapshotsCommands

  override def persistenceId: String = "matcher"

  private def createBalanceWatcher(): Unit = if (settings.balanceWatching.enable) {
    val balanceWatcherMaster =
      context.actorOf(FromConfig.props(BalanceWatcherWorkerActor.props(settings.balanceWatching, self, orderHistory)), "balance-watcher-router")
    context.system.eventStream.subscribe(balanceWatcherMaster, classOf[BalanceChanged])
  }
}

object MatcherActor {
  def name = "matcher"

  def props(orderHistoryActor: ActorRef,
            pairBuilder: AssetPairBuilder,
            orderBooks: AtomicReference[Map[AssetPair, ActorRef]],
            updateSnapshot: AssetPair => OrderBook => Unit,
            wallet: Wallet,
            utx: UtxPool,
            allChannels: ChannelGroup,
            settings: MatcherSettings,
            blockchain: Blockchain,
            functionalitySettings: FunctionalitySettings): Props =
    Props(
      new MatcherActor(orderHistoryActor,
                       pairBuilder,
                       orderBooks,
                       updateSnapshot,
                       wallet,
                       utx,
                       allChannels,
                       settings,
                       blockchain,
                       functionalitySettings))

  case object SaveSnapshot

  case class Snapshot(tradedPairsSet: Set[AssetPair])

  case class OrderBookCreated(pair: AssetPair)

  case object GetMarkets

  case object Shutdown

  case object ShutdownComplete

  case class GetMarketsResponse(publicKey: Array[Byte], markets: Seq[MarketData]) extends MatcherResponse {
    def getMarketsJs: JsValue =
      JsArray(
        markets.map(m =>
          Json.obj(
            "amountAsset"     -> m.pair.amountAssetStr,
            "amountAssetName" -> m.amountAssetName,
            "amountAssetInfo" -> m.amountAssetInfo,
            "priceAsset"      -> m.pair.priceAssetStr,
            "priceAssetName"  -> m.priceAssetName,
            "priceAssetInfo"  -> m.priceAssetinfo,
            "created"         -> m.created
        )))

    def json: JsValue = Json.obj(
      "matcherPublicKey" -> Base58.encode(publicKey),
      "markets"          -> getMarketsJs
    )

    def code: StatusCode = StatusCodes.OK
  }

  case class AssetInfo(decimals: Int)
  implicit val assetInfoFormat: Format[AssetInfo] = Json.format[AssetInfo]

  case class MarketData(pair: AssetPair,
                        amountAssetName: String,
                        priceAssetName: String,
                        created: Long,
                        amountAssetInfo: Option[AssetInfo],
                        priceAssetinfo: Option[AssetInfo])

  def compare(buffer1: Option[Array[Byte]], buffer2: Option[Array[Byte]]): Int = {
    if (buffer1.isEmpty && buffer2.isEmpty) 0
    else if (buffer1.isEmpty) -1
    else if (buffer2.isEmpty) 1
    else ByteArray.compare(buffer1.get, buffer2.get)
  }

  class GracefulShutdownActor(children: Vector[ActorRef], receiver: ActorRef) extends Actor {
    children.foreach(_ ! Shutdown)

    override def receive: Receive = state(children.size)

    private def state(expectedResponses: Int): Receive = {
      case ShutdownComplete =>
        if (expectedResponses > 1) context.become(state(expectedResponses - 1))
        else {
          receiver ! ShutdownComplete
          context.stop(self)
        }
    }
  }
}
