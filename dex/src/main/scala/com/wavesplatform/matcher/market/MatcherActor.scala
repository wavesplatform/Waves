package com.wavesplatform.matcher.market

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, Props, SupervisorStrategy, Terminated}
import akka.persistence._
import com.google.common.base.Charsets
import com.wavesplatform.matcher.api.{DuringShutdown, OrderBookUnavailable}
import com.wavesplatform.matcher.error.MatcherError
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.queue.QueueEventWithMeta.{Offset => EventOffset}
import com.wavesplatform.matcher.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.matcher.settings.MatcherSettings
import com.wavesplatform.state.AssetDescription
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.json._
import scorex.utils._

class MatcherActor(settings: MatcherSettings,
                   recoveryCompletedWithEventNr: Either[String, (ActorRef, Long)] => Unit,
                   orderBooks: AtomicReference[Map[AssetPair, Either[Unit, ActorRef]]],
                   orderBookActorProps: (AssetPair, ActorRef) => Props,
                   assetDescription: IssuedAsset => Option[AssetDescription])
    extends PersistentActor
    with ScorexLogging {

  import MatcherActor._

  override def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private var tradedPairs: Map[AssetPair, MarketData] = Map.empty
  private var childrenNames: Map[ActorRef, AssetPair] = Map.empty
  private var lastSnapshotSequenceNr: Long            = -1L
  private var lastProcessedNr: Long                   = -1L

  private var snapshotsState = SnapshotsState.empty

  private var shutdownStatus: ShutdownStatus = ShutdownStatus(
    initiated = false,
    oldMessagesDeleted = false,
    oldSnapshotsDeleted = false,
    onComplete = () => ()
  )

  private def orderBook(pair: AssetPair) = Option(orderBooks.get()).flatMap(_.get(pair))

  private def getAssetName(asset: Asset, desc: Option[AssetDescription]): String =
    asset match {
      case Waves => AssetPair.WavesName
      case _     => desc.fold("Unknown")(d => new String(d.name, Charsets.UTF_8))
    }

  private def getAssetInfo(asset: Asset, desc: Option[AssetDescription]): Option[AssetInfo] =
    asset.fold(Option(8))(_ => desc.map(_.decimals)).map(AssetInfo)

  private def getAssetDescriptionByAssetId(assetId: Asset): Option[AssetDescription] = assetId match {
    case Waves                  => None
    case asset @ IssuedAsset(_) => assetDescription(asset)
  }

  private def createMarketData(pair: AssetPair): MarketData = {
    val amountDesc = getAssetDescriptionByAssetId(pair.amountAsset)
    val priceDesc  = getAssetDescriptionByAssetId(pair.priceAsset)

    MarketData(
      pair,
      getAssetName(pair.amountAsset, amountDesc),
      getAssetName(pair.priceAsset, priceDesc),
      System.currentTimeMillis(),
      getAssetInfo(pair.amountAsset, amountDesc),
      getAssetInfo(pair.priceAsset, priceDesc)
    )
  }

  private def createOrderBook(pair: AssetPair): ActorRef = {
    log.info(s"Creating order book for $pair")
    val orderBook = context.watch(context.actorOf(orderBookActorProps(pair, self), OrderBookActor.name(pair)))
    childrenNames += orderBook -> pair
    orderBooks.updateAndGet(_ + (pair -> Right(orderBook)))
    tradedPairs += pair -> createMarketData(pair)
    orderBook
  }

  /**
    * @param f (sender, orderBook)
    */
  private def runFor(assetPair: AssetPair, autoCreate: Boolean = true)(f: (ActorRef, ActorRef) => Unit): Unit = {
    val s = sender()
    if (shutdownStatus.initiated) s ! DuringShutdown
    else
      orderBook(assetPair) match {
        case Some(Right(ob)) => f(s, ob)
        case Some(Left(_))   => s ! OrderBookUnavailable(MatcherError.OrderBookUnavailable(assetPair))
        case None =>
          if (autoCreate) {
            val ob = createOrderBook(assetPair)
            persistAsync(OrderBookCreated(assetPair))(_ => ())
            f(s, ob)
          } else {
            log.warn(s"OrderBook fro $assetPair is stopped and autoCreate is $autoCreate, respond to client with OrderBookUnavailable")
            s ! OrderBookUnavailable
          }
      }
  }

  private def createSnapshotFor(offset: QueueEventWithMeta.Offset): Unit = {
    snapshotsState.requiredSnapshot(offset).foreach {
      case (assetPair, updatedSnapshotState) =>
        orderBooks.get.get(assetPair) match {
          case Some(Right(actorRef)) =>
            log.info(
              s"The $assetPair order book should do a snapshot, the current offset is $offset. The next snapshot candidate: ${updatedSnapshotState.nearestSnapshotOffset}")
            actorRef ! SaveSnapshot(offset)

          case Some(Left(_)) =>
            log.warn(s"Can't create a snapshot for $assetPair: the order book is down, ignoring it in the snapshot's rotation.")

          case None =>
            log.warn(s"Can't create a snapshot for $assetPair: the order book has't yet started or was removed.")
        }
        snapshotsState = updatedSnapshotState
    }
  }

  private def forwardToOrderBook: Receive = {
    case GetMarkets => sender() ! tradedPairs.values.toSeq

    case GetSnapshotOffsets => sender() ! SnapshotOffsetsResponse(snapshotsState.snapshotOffsets)

    case request: QueueEventWithMeta =>
      request.event match {
        case QueueEvent.OrderBookDeleted(assetPair) =>
          // autoCreate = false for case, when multiple OrderBookDeleted(A1-A2) events happen one after another
          runFor(request.event.assetPair, autoCreate = false) { (sender, ref) =>
            ref.tell(request, sender)
            orderBooks.getAndUpdate(_.filterNot { x =>
              x._2.right.exists(_ == ref)
            })

            tradedPairs -= assetPair
          }

        case _ => runFor(request.event.assetPair)((sender, orderBook) => orderBook.tell(request, sender))
      }
      lastProcessedNr = math.max(request.offset, lastProcessedNr)
      createSnapshotFor(lastProcessedNr)

    case request: ForceStartOrderBook =>
      runFor(request.assetPair)((sender, orderBook) => orderBook.tell(request, sender))

    case Shutdown =>
      shutdownStatus = shutdownStatus.copy(initiated = true, onComplete = () => context.stop(self))

      context.children.foreach(context.unwatch)
      context.become(snapshotsCommands orElse shutdownFallback)

      if (lastSnapshotSequenceNr < lastSequenceNr) saveSnapshot(Snapshot(tradedPairs.keySet))
      else {
        log.debug(s"No changes, lastSnapshotSequenceNr = $lastSnapshotSequenceNr, lastSequenceNr = $lastSequenceNr")
        shutdownStatus = shutdownStatus.copy(
          oldMessagesDeleted = true,
          oldSnapshotsDeleted = true
        )
        shutdownStatus.tryComplete()
      }

    case Terminated(ref) =>
      log.error(s"$ref is terminated")
      orderBooks.getAndUpdate { m =>
        childrenNames.get(ref).fold(m)(m.updated(_, Left(())))
      }

    case OrderBookRecovered(assetPair, eventNr) =>
      snapshotsState = snapshotsState.updated(assetPair, eventNr, lastProcessedNr, settings.snapshotsInterval)

    case OrderBookSnapshotUpdated(assetPair, eventNr) =>
      snapshotsState = snapshotsState.updated(assetPair, Some(eventNr), lastProcessedNr, settings.snapshotsInterval)
  }

  override def receiveRecover: Receive = {
    case event @ OrderBookCreated(pair) =>
      if (orderBook(pair).isEmpty) {
        log.debug(s"Replaying event $event")
        createOrderBook(pair)
      }

    case SnapshotOffer(metadata, snapshot: Snapshot) =>
      lastSnapshotSequenceNr = metadata.sequenceNr
      log.info(s"Loaded the snapshot with nr = ${metadata.sequenceNr}")
      snapshot.tradedPairsSet.foreach(createOrderBook)

    case RecoveryCompleted =>
      if (orderBooks.get().isEmpty) {
        log.info("Recovery completed!")
        recoveryCompletedWithEventNr(Right((self, -1L)))
      } else {
        val obs = orderBooks.get()
        log.info(s"Recovery completed, waiting order books to restore: ${obs.keys.mkString(", ")}")
        context.become(collectOrderBooks(obs.size, None, -1L, Map.empty))
      }
  }

  private def collectOrderBooks(restOrderBooksNumber: Long,
                                oldestEventNr: Option[Long],
                                newestEventNr: Long,
                                currentOffsets: Map[AssetPair, Option[EventOffset]]): Receive = {
    case OrderBookRecovered(assetPair, snapshotEventNr) =>
      val updatedRestOrderBooksNumber = restOrderBooksNumber - 1

      val updatedOldestSnapshotOffset = (oldestEventNr, snapshotEventNr) match {
        case (Some(oldestNr), Some(orderBookNr)) => Some(math.min(oldestNr, orderBookNr))
        case (oldestNr, orderBookNr)             => oldestNr.orElse(orderBookNr)
      }

      val updatedNewestEventNr  = math.max(newestEventNr, snapshotEventNr.getOrElse(-1L))
      val updatedCurrentOffsets = currentOffsets.updated(assetPair, snapshotEventNr)

      if (updatedRestOrderBooksNumber > 0)
        context.become(collectOrderBooks(updatedRestOrderBooksNumber, updatedOldestSnapshotOffset, updatedNewestEventNr, updatedCurrentOffsets))
      else becomeWorking(updatedOldestSnapshotOffset, updatedNewestEventNr, updatedCurrentOffsets)

    case Terminated(ref) =>
      context.stop(self)
      recoveryCompletedWithEventNr(Left(s"$ref is terminated"))

    case Shutdown =>
      context.children.foreach(context.unwatch)
      context.stop(self)
      recoveryCompletedWithEventNr(Left("Received Shutdown command"))

    case _ => stash()
  }

  private def becomeWorking(oldestEventNr: Option[EventOffset],
                            newestEventNr: EventOffset,
                            currentOffsets: Map[AssetPair, Option[EventOffset]]): Unit = {
    context.become(receiveCommand)

    // If oldestEventNr <= snapshotsInterval, there could be a situation:
    // 1. There was an event with offset=N for order book X
    // 2. A snapshot for X wasn't created at the moment of last event, but it was created for order book Y at offset=N+2
    // 3. After restart we ignore the event with offset=N, starting from offset=N+1
    // So we need to start from the nearest snapshot interval start point
    val processedEventNr = oldestEventNr.fold(0L)(_ / settings.snapshotsInterval * settings.snapshotsInterval) - 1L

    snapshotsState = SnapshotsState(
      currentOffsets = currentOffsets,
      lastProcessedOffset = newestEventNr,
      interval = settings.snapshotsInterval
    )

    log.info(s"All snapshots are loaded, oldestEventNr: $oldestEventNr, processedEventNr: $processedEventNr, newestEventNr: $newestEventNr")
    log.trace(s"Expecting next snapshots at: ${snapshotsState.nearestSnapshotOffsets}")

    unstashAll()
    recoveryCompletedWithEventNr(Right((self, processedEventNr)))
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
    case _ if shutdownStatus.initiated => sender() ! DuringShutdown
  }

  override def receiveCommand: Receive = forwardToOrderBook orElse snapshotsCommands

  override def persistenceId: String = MatcherActor.name
}

object MatcherActor {
  def name: String = "matcher"

  def props(matcherSettings: MatcherSettings,
            recoveryCompletedWithEventNr: Either[String, (ActorRef, Long)] => Unit,
            orderBooks: AtomicReference[Map[AssetPair, Either[Unit, ActorRef]]],
            orderBookProps: (AssetPair, ActorRef) => Props,
            assetDescription: IssuedAsset => Option[AssetDescription]): Props =
    Props(
      new MatcherActor(
        matcherSettings,
        recoveryCompletedWithEventNr,
        orderBooks,
        orderBookProps,
        assetDescription
      ))

  private case class ShutdownStatus(initiated: Boolean, oldMessagesDeleted: Boolean, oldSnapshotsDeleted: Boolean, onComplete: () => Unit) {
    def completed: ShutdownStatus = copy(
      initiated = true,
      oldMessagesDeleted = true,
      oldSnapshotsDeleted = true
    )
    def isCompleted: Boolean = initiated && oldMessagesDeleted && oldSnapshotsDeleted
    def tryComplete(): Unit  = if (isCompleted) onComplete()
  }

  case class SaveSnapshot(globalEventNr: EventOffset)

  case class Snapshot(tradedPairsSet: Set[AssetPair])

  case class ForceStartOrderBook(assetPair: AssetPair)
  case class OrderBookCreated(assetPair: AssetPair)

  case object GetMarkets

  case object GetSnapshotOffsets
  case class SnapshotOffsetsResponse(offsets: Map[AssetPair, Option[EventOffset]])

  case class MatcherRecovered(oldestEventNr: Long)

  case object Shutdown

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
}
