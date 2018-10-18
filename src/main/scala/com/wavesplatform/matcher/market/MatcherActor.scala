package com.wavesplatform.matcher.market

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorRef, PoisonPill, Props, Terminated}
import akka.http.scaladsl.model._
import akka.persistence.{PersistentActor, RecoveryCompleted, _}
import com.google.common.base.Charsets
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.MatcherResponse
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.model.Events.OrderExecuted
import com.wavesplatform.matcher.model.OrderBook
import com.wavesplatform.state.{AssetDescription, ByteStr}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.wavesplatform.transaction.{AssetId, ValidationError}
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import play.api.libs.json._
import scorex.utils._

class MatcherActor(validateAssetPair: AssetPair => Either[String, AssetPair],
                   orderBooks: AtomicReference[Map[AssetPair, ActorRef]],
                   updateSnapshot: AssetPair => OrderBook => Unit,
                   utx: UtxPool,
                   allChannels: ChannelGroup,
                   settings: MatcherSettings,
                   assetDescription: ByteStr => Option[AssetDescription],
                   createTransaction: OrderExecuted => Either[ValidationError, ExchangeTransaction])
    extends PersistentActor
    with ScorexLogging {

  import MatcherActor._

  private var tradedPairs            = Map.empty[AssetPair, MarketData]
  private var lastSnapshotSequenceNr = 0L

  private var shutdownStatus: ShutdownStatus = ShutdownStatus(
    initiated = false,
    orderBooksStopped = false,
    oldMessagesDeleted = false,
    oldSnapshotsDeleted = false,
    onComplete = () => ()
  )

  private def orderBook(pair: AssetPair) = Option(orderBooks.get()).flatMap(_.get(pair))

  def getAssetName(asset: Option[AssetId], desc: Option[AssetDescription]): String =
    asset.fold(AssetPair.WavesName) { _ =>
      desc.fold("Unknown")(d => new String(d.name, Charsets.UTF_8))
    }

  def getAssetInfo(asset: Option[AssetId], desc: Option[AssetDescription]): Option[AssetInfo] =
    asset.fold(Option(8))(_ => desc.map(_.decimals)).map(AssetInfo)

  private def createMarketData(pair: AssetPair): MarketData = {
    val amountDesc = pair.amountAsset.flatMap(assetDescription)
    val priceDesc  = pair.priceAsset.flatMap(assetDescription)

    MarketData(
      pair,
      getAssetName(pair.amountAsset, amountDesc),
      getAssetName(pair.priceAsset, priceDesc),
      System.currentTimeMillis(),
      getAssetInfo(pair.amountAsset, amountDesc),
      getAssetInfo(pair.priceAsset, priceDesc)
    )
  }

  private def createOrderBookActor(pair: AssetPair): ActorRef = context.actorOf(
    OrderBookActor.props(pair, updateSnapshot(pair), utx, allChannels, settings, createTransaction),
    OrderBookActor.name(pair)
  )

  def createOrderBook(pair: AssetPair): ActorRef = {
    log.info(s"Creating order book for $pair")
    val orderBook = createOrderBookActor(pair)
    orderBooks.updateAndGet(_ + (pair -> orderBook))
    tradedPairs += pair -> createMarketData(pair)
    orderBook
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

  def forwardToOrderBook: Receive = {
    case GetMarkets => sender() ! tradedPairs.values.toSeq

    case req @ GetMarketStatusRequest(pair) =>
      context
        .child(OrderBookActor.name(pair))
        .fold(sender() ! MatcherResponse(StatusCodes.NotFound, "Market not found"))(forwardReq(req))

    case order: Order =>
      orderBook(order.assetPair).fold(createAndForward(order))(forwardReq(order))

    case ob: DeleteOrderBookRequest =>
      orderBook(ob.assetPair).fold(returnEmptyOrderBook(ob.assetPair))(forwardReq(ob))
      removeOrderBook(ob.assetPair)

    case Shutdown =>
      shutdownStatus = shutdownStatus.copy(
        initiated = true,
        onComplete = { () =>
          context.stop(self)
        }
      )

      context.become(snapshotsCommands orElse shutdownFallback)

      if (lastSnapshotSequenceNr < lastSequenceNr) saveSnapshot(Snapshot(tradedPairs.keySet))
      else {
        log.debug(s"No changes, lastSnapshotSequenceNr = $lastSnapshotSequenceNr, lastSequenceNr = $lastSequenceNr")
        shutdownStatus = shutdownStatus.copy(
          oldMessagesDeleted = true,
          oldSnapshotsDeleted = true
        )
      }

      if (context.children.isEmpty) {
        shutdownStatus = shutdownStatus.copy(orderBooksStopped = true)
        shutdownStatus.tryComplete()
      } else {
        context.actorOf(Props(classOf[GracefulShutdownActor], context.children.toVector, self))
      }
  }

  private def removeOrderBook(pair: AssetPair): Unit = {
    if (tradedPairs.contains(pair)) {
      tradedPairs -= pair
      deleteMessages(lastSequenceNr)
      saveSnapshot(Snapshot(tradedPairs.keySet))
    }
  }

  override def receiveRecover: Receive = {
    case OrderBookCreated(pair) =>
      if (orderBook(pair).isEmpty) {
        log.info(s"Order book created for $pair")
        createOrderBook(pair)
      }

    case SnapshotOffer(metadata, snapshot: Snapshot) =>
      lastSnapshotSequenceNr = metadata.sequenceNr
      log.info(s"Loaded the snapshot with nr = ${metadata.sequenceNr}")
      snapshot.tradedPairsSet.foreach(createOrderBook)

    case RecoveryCompleted =>
      log.info("Recovery completed!")
  }

  private def snapshotsCommands: Receive = {
    case SaveSnapshotSuccess(metadata) =>
      lastSnapshotSequenceNr = metadata.sequenceNr
      log.info(s"Snapshot saved with metadata $metadata")
      deleteMessages(metadata.sequenceNr - 1)
      deleteSnapshots(SnapshotSelectionCriteria.Latest.copy(maxSequenceNr = metadata.sequenceNr - 1))

    case SaveSnapshotFailure(metadata, reason) =>
      log.error(s"Failed to save snapshot: $metadata, $reason.")
      if (shutdownStatus.initiated) {
        shutdownStatus = shutdownStatus.copy(
          oldMessagesDeleted = true,
          oldSnapshotsDeleted = true
        )
        shutdownStatus.tryComplete()
      }

    case DeleteMessagesSuccess(nr) =>
      log.info(s"Old messages are deleted up to $nr")
      if (shutdownStatus.initiated) {
        shutdownStatus = shutdownStatus.copy(oldMessagesDeleted = true)
        shutdownStatus.tryComplete()
      }

    case DeleteMessagesFailure(cause, nr) =>
      log.info(s"Failed to delete messages up to $nr: $cause")
      if (shutdownStatus.initiated) {
        shutdownStatus = shutdownStatus.copy(oldMessagesDeleted = true)
        shutdownStatus.tryComplete()
      }

    case DeleteSnapshotsSuccess(nr) =>
      log.info(s"Old snapshots are deleted up to $nr")
      if (shutdownStatus.initiated) {
        shutdownStatus = shutdownStatus.copy(oldSnapshotsDeleted = true)
        shutdownStatus.tryComplete()
      }

    case DeleteSnapshotsFailure(cause, nr) =>
      log.info(s"Failed to delete old snapshots to $nr: $cause")
      if (shutdownStatus.initiated) {
        shutdownStatus = shutdownStatus.copy(oldSnapshotsDeleted = true)
        shutdownStatus.tryComplete()
      }
  }

  private def shutdownFallback: Receive = {
    case ShutdownComplete =>
      shutdownStatus = shutdownStatus.copy(orderBooksStopped = true)
      shutdownStatus.tryComplete()

    case _ if shutdownStatus.initiated => sender() ! MatcherResponse(StatusCodes.ServiceUnavailable, "System is going shutdown")
  }

  override def receiveCommand: Receive = forwardToOrderBook orElse snapshotsCommands

  override def persistenceId: String = "matcher"
}

object MatcherActor {
  def name = "matcher"

  def props(validateAssetPair: AssetPair => Either[String, AssetPair],
            orderBooks: AtomicReference[Map[AssetPair, ActorRef]],
            updateSnapshot: AssetPair => OrderBook => Unit,
            utx: UtxPool,
            allChannels: ChannelGroup,
            settings: MatcherSettings,
            assetDescription: ByteStr => Option[AssetDescription],
            createTransaction: OrderExecuted => Either[ValidationError, ExchangeTransaction]): Props =
    Props(new MatcherActor(validateAssetPair, orderBooks, updateSnapshot, utx, allChannels, settings, assetDescription, createTransaction))

  private case class ShutdownStatus(initiated: Boolean,
                                    oldMessagesDeleted: Boolean,
                                    oldSnapshotsDeleted: Boolean,
                                    orderBooksStopped: Boolean,
                                    onComplete: () => Unit) {
    def completed: ShutdownStatus = copy(
      initiated = true,
      oldMessagesDeleted = true,
      oldSnapshotsDeleted = true,
      orderBooksStopped = true
    )
    def isCompleted: Boolean = initiated && oldMessagesDeleted && oldSnapshotsDeleted && orderBooksStopped
    def tryComplete(): Unit  = if (isCompleted) onComplete()
  }

  case object SaveSnapshot

  case class Snapshot(tradedPairsSet: Set[AssetPair])

  case class OrderBookCreated(pair: AssetPair)

  case object GetMarkets

  case object Shutdown

  case object ShutdownComplete

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
    children.map(context.watch).foreach(_ ! PoisonPill)

    override def receive: Receive = state(children.size)

    private def state(expectedResponses: Int): Receive = {
      case _: Terminated =>
        if (expectedResponses > 1) context.become(state(expectedResponses - 1))
        else {
          receiver ! ShutdownComplete
          context.stop(self)
        }
    }
  }
}
