package com.wavesplatform.matcher.market

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, Props, SupervisorStrategy, Terminated}
import akka.persistence._
import com.google.common.base.Charsets
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.{DuringShutdown, OrderBookUnavailable}
import com.wavesplatform.matcher.error.MatcherError
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.queue.QueueEventWithMeta.{Offset => EventOffset}
import com.wavesplatform.matcher.queue.{QueueEvent, QueueEventWithMeta}
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
  private var lastSnapshotSequenceNr: Long            = 0L
  private var lastProcessedNr: Long                   = 0L

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
  private def runFor(eventWithMeta: QueueEventWithMeta)(f: (ActorRef, ActorRef) => Unit): Unit = {
    import eventWithMeta.event.assetPair
    import eventWithMeta.offset

    val s = sender()
    if (shutdownStatus.initiated) s ! DuringShutdown
    else
      orderBook(assetPair) match {
        case Some(Right(ob)) =>
          f(s, ob)
          snapshotsState.requiredSnapshot(offset).foreach {
            case (assetPair, updatedSnapshotState) =>
              log.info(
                s"OrderBook $assetPair should do snapshot, the current offset is $offset. Next snapshot: ${updatedSnapshotState.nearestSnapshotOffset}")
              orderBooks.get.get(assetPair).flatMap(_.toOption).foreach(_ ! SaveSnapshot(offset))
              snapshotsState = updatedSnapshotState
          }
        case Some(Left(_)) => s ! OrderBookUnavailable(MatcherError.OrderBookUnavailable(assetPair))
        case None =>
          val ob = createOrderBook(assetPair)
          persistAsync(OrderBookCreated(assetPair))(_ => ())
          f(s, ob)
      }
  }

  private def forwardToOrderBook: Receive = {
    case GetMarkets => sender() ! tradedPairs.values.toSeq

    case GetSnapshotOffsets => sender() ! SnapshotOffsetsResponse(snapshotsState.snapshotOffsets)

    case request: QueueEventWithMeta =>
      request.event match {
        case QueueEvent.OrderBookDeleted(assetPair) =>
          runFor(request) { (sender, ref) =>
            ref.tell(request, sender)
            orderBooks.getAndUpdate(_.filterNot { x =>
              x._2.right.exists(_ == ref)
            })

            tradedPairs -= assetPair
          }

        case _ => runFor(request)((sender, orderBook) => orderBook.tell(request, sender))
      }
      lastProcessedNr = math.max(request.offset, lastProcessedNr)

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

    case OrderBookSnapshotUpdated(assetPair, eventNr) =>
      snapshotsState = snapshotsState.updated(assetPair, eventNr, lastProcessedNr, settings.snapshotsInterval)
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
        recoveryCompletedWithEventNr(Right((self, -1)))
      } else {
        val obs = orderBooks.get()
        log.info(s"Recovery completed, waiting order books to restore: ${obs.keys.mkString(", ")}")
        context.become(collectOrderBooks(obs.size, Long.MaxValue, Long.MinValue, Map.empty))
      }
  }

  private def collectOrderBooks(restOrderBooksNumber: Long,
                                oldestEventNr: Long,
                                newestEventNr: Long,
                                currentOffsets: Map[AssetPair, EventOffset]): Receive = {
    case OrderBookSnapshotUpdated(assetPair, snapshotEventNr) =>
      log.info(s"Last snapshot for $assetPair did at $snapshotEventNr")

      val updatedRestOrderBooksNumber = restOrderBooksNumber - 1
      val updatedOldestEventNr        = math.min(oldestEventNr, snapshotEventNr)
      val updatedNewestEventNr        = math.max(newestEventNr, snapshotEventNr)
      val updatedCurrentOffsets       = currentOffsets.updated(assetPair, snapshotEventNr)

      if (updatedRestOrderBooksNumber > 0)
        context.become(collectOrderBooks(updatedRestOrderBooksNumber, updatedOldestEventNr, updatedNewestEventNr, updatedCurrentOffsets))
      else becomeWorking(updatedOldestEventNr, updatedNewestEventNr, updatedCurrentOffsets)

    case Terminated(ref) =>
      context.stop(self)
      recoveryCompletedWithEventNr(Left(s"$ref is terminated"))

    case Shutdown =>
      context.children.foreach(context.unwatch)
      context.stop(self)
      recoveryCompletedWithEventNr(Left("Received Shutdown command"))

    case _ => stash()
  }

  private def becomeWorking(oldestEventNr: EventOffset, newestEventNr: EventOffset, currentOffsets: Map[AssetPair, EventOffset]): Unit = {
    context.become(receiveCommand)

    snapshotsState = SnapshotsState(
      startOffsetToSnapshot = lastProcessedNr,
      currentOffsets = currentOffsets,
      lastProcessedNr = lastProcessedNr,
      interval = settings.snapshotsInterval
    )

    log.info(s"All snapshots are loaded, oldestEventNr: $oldestEventNr, newestEventNr: $newestEventNr")
    log.trace(s"Expecting snapshots at: ${snapshotsState.nearestSnapshotOffsets}")

    unstashAll()
    recoveryCompletedWithEventNr(Right((self, oldestEventNr)))
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

  override def persistenceId: String = "matcher"
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

  case class OrderBookCreated(pair: AssetPair)

  case object GetMarkets

  case object GetSnapshotOffsets
  case class SnapshotOffsetsResponse(offsets: Map[AssetPair, EventOffset])

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
