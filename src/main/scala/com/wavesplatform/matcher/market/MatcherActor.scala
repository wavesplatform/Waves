package com.wavesplatform.matcher.market

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorRef, PoisonPill, Props, SupervisorStrategy, Terminated}
import akka.persistence.{PersistentActor, RecoveryCompleted, _}
import com.google.common.base.Charsets
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.{DuringShutdown, OrderBookUnavailable}
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.state.{AssetDescription, ByteStr}
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.json._
import scorex.utils._

class MatcherActor(matcherSettings: MatcherSettings,
                   recoveryCompletedWithCommandNr: (ActorRef, Long, Long) => Unit,
                   orderBooks: AtomicReference[Map[AssetPair, Either[Unit, ActorRef]]],
                   orderBookActorProps: (AssetPair, ActorRef) => Props,
                   assetDescription: ByteStr => Option[AssetDescription])
    extends PersistentActor
    with ScorexLogging {

  import MatcherActor._

  override def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private var tradedPairs: Map[AssetPair, MarketData] = Map.empty
  private var childrenNames: Map[ActorRef, AssetPair] = Map.empty
  private var lastSnapshotSequenceNr: Long            = 0L

  private var snapshotOffsets: Map[AssetPair, QueueEventWithMeta.Offset] = Map.empty

  private var shutdownStatus: ShutdownStatus = ShutdownStatus(
    initiated = false,
    orderBooksStopped = false,
    oldMessagesDeleted = false,
    oldSnapshotsDeleted = false,
    onComplete = () => ()
  )

  private def orderBook(pair: AssetPair) = Option(orderBooks.get()).flatMap(_.get(pair))

  private def getAssetName(asset: Option[AssetId], desc: Option[AssetDescription]): String =
    asset.fold(AssetPair.WavesName) { _ =>
      desc.fold("Unknown")(d => new String(d.name, Charsets.UTF_8))
    }

  private def getAssetInfo(asset: Option[AssetId], desc: Option[AssetDescription]): Option[AssetInfo] =
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

  private def createOrderBookActor(pair: AssetPair): ActorRef = {
    val r = context.actorOf(orderBookActorProps(pair, self), OrderBookActor.name(pair))
    childrenNames += r -> pair
    context.watch(r)
    r
  }

  private def createOrderBook(pair: AssetPair): ActorRef = {
    log.info(s"Creating order book for $pair")
    val orderBook = createOrderBookActor(pair)
    orderBooks.updateAndGet(_ + (pair -> Right(orderBook)))
    tradedPairs += pair -> createMarketData(pair)
    orderBook
  }

  private def snapshotOffset(pair: AssetPair): Long = {
    val half = matcherSettings.snapshotsInterval / 2
    half + (pair.key.hashCode % half)
  }

  private var minSnapshotOffset = Long.MinValue
  private def shouldDoSnapshot(request: QueueEventWithMeta): Boolean = {
    import request.event.assetPair
    request.offset > minSnapshotOffset && snapshotOffsets.get(assetPair).fold(false) { oldestSnapshotNr =>
      val minOffset = oldestSnapshotNr + snapshotOffset(assetPair)
      val r         = request.offset >= minOffset
      if (r) log.info(s"$assetPair should do a snapshot at $minOffset, now is ${request.offset}")
      r
    }
  }

  /**
    * @param f (sender, orderBook)
    */
  private def runFor(eventWithMeta: QueueEventWithMeta)(f: (ActorRef, ActorRef) => Unit): Unit = {
    import eventWithMeta.event.assetPair
    val s = sender()
    if (shutdownStatus.initiated) s ! DuringShutdown
    else
      orderBook(assetPair) match {
        case Some(Right(ob)) =>
          f(s, ob)
          if (shouldDoSnapshot(eventWithMeta)) {
            snapshotOffsets -= assetPair
            ob ! SaveSnapshot
          }
        case Some(Left(_)) => s ! OrderBookUnavailable
        case None =>
          val ob = createOrderBook(assetPair)
          persistAsync(OrderBookCreated(assetPair))(_ => ())
          f(s, ob)
      }
  }

  private def forwardToOrderBook: Receive = {
    case GetMarkets => sender() ! tradedPairs.values.toSeq

    case GetSnapshotOffsets => sender() ! SnapshotOffsetsResponse(snapshotOffsets)

    case request: QueueEventWithMeta =>
      request.event match {
        case QueueEvent.OrderBookDeleted(assetPair) =>
          runFor(request) { (sender, ref) =>
            ref.tell(request, sender)
            orderBooks.getAndUpdate(_.filterNot { x =>
              x._2.right.exists(_ == ref)
            })

            tradedPairs -= assetPair
            deleteMessages(lastSequenceNr)
            saveSnapshot(Snapshot(tradedPairs.keySet))
          }

        case _ => runFor(request)((sender, orderBook) => orderBook.tell(request, sender))
      }

    case Shutdown =>
      shutdownStatus = shutdownStatus.copy(
        initiated = true,
        onComplete = { () =>
          context.stop(self)
        }
      )

      context.children.foreach(context.unwatch)
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
        context.actorOf(Props(new GracefulShutdownActor(context.children.toVector, self)))
      }

    case Terminated(ref) =>
      orderBooks.getAndUpdate { m =>
        childrenNames.get(ref).fold(m)(m.updated(_, Left(())))
      }

    case OrderBookSnapshotUpdated(assetPair, lastProcessedCommandNr) =>
      snapshotOffsets += assetPair -> lastProcessedCommandNr
  }

  override def receiveRecover: Receive = {
    case OrderBookCreated(pair) => if (orderBook(pair).isEmpty) createOrderBook(pair)

    case SnapshotOffer(metadata, snapshot: Snapshot) =>
      lastSnapshotSequenceNr = metadata.sequenceNr
      log.info(s"Loaded the snapshot with nr = ${metadata.sequenceNr}")
      snapshot.tradedPairsSet.foreach(createOrderBook)

    case RecoveryCompleted =>
      if (orderBooks.get().isEmpty) {
        log.info("Recovery completed!")
        recoveryCompletedWithCommandNr(self, -1, -1)
      } else {
        log.info(s"Recovery completed, waiting order books to restore: ${orderBooks.get().keys.mkString(", ")}")
        context.become(collectOrderBooks(orderBooks.get().size, Long.MaxValue, Long.MinValue))
      }
  }

  private def collectOrderBooks(restOrderBooksNumber: Long, oldestCommandNr: Long, newestCommandNr: Long): Receive = {
    case OrderBookSnapshotUpdated(assetPair, lastProcessedCommandNr) =>
      val updatedRestOrderBooksNumber = restOrderBooksNumber - 1
      val updatedOldestCommandNr      = math.min(oldestCommandNr, lastProcessedCommandNr)
      val updatedNewestCommandNr      = math.max(newestCommandNr, lastProcessedCommandNr)

      snapshotOffsets += assetPair -> lastProcessedCommandNr

      if (updatedRestOrderBooksNumber > 0)
        context.become(collectOrderBooks(updatedRestOrderBooksNumber, updatedOldestCommandNr, updatedNewestCommandNr))
      else {
        context.become(receiveCommand)
        minSnapshotOffset = updatedNewestCommandNr
        recoveryCompletedWithCommandNr(self, updatedOldestCommandNr, updatedNewestCommandNr)
        unstashAll()
      }

    case Terminated(ref) =>
      orderBooks.getAndUpdate { m =>
        childrenNames.get(ref).fold(m)(m.updated(_, Left(())))
      }

      val updatedRestOrderBooksNumber = restOrderBooksNumber - 1
      if (updatedRestOrderBooksNumber > 0) context.become(collectOrderBooks(updatedRestOrderBooksNumber, oldestCommandNr, newestCommandNr))
      else {
        context.become(receiveCommand)
        minSnapshotOffset = newestCommandNr
        recoveryCompletedWithCommandNr(self, oldestCommandNr, newestCommandNr)
        unstashAll()
      }

    case _ => stash()
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

    case _ if shutdownStatus.initiated => sender() ! DuringShutdown
  }

  override def receiveCommand: Receive = forwardToOrderBook orElse snapshotsCommands

  override def persistenceId: String = "matcher"
}

object MatcherActor {
  def name: String = "matcher"

  def props(matcherSettings: MatcherSettings,
            recoveryCompletedWithCommandNr: (ActorRef, Long, Long) => Unit,
            orderBooks: AtomicReference[Map[AssetPair, Either[Unit, ActorRef]]],
            orderBookProps: (AssetPair, ActorRef) => Props,
            assetDescription: ByteStr => Option[AssetDescription]): Props =
    Props(
      new MatcherActor(
        matcherSettings,
        recoveryCompletedWithCommandNr,
        orderBooks,
        orderBookProps,
        assetDescription
      ))

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

  case object GetSnapshotOffsets
  case class SnapshotOffsetsResponse(offsets: Map[AssetPair, QueueEventWithMeta.Offset])

  case class MatcherRecovered(oldestCommandNr: Long)

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
