package com.wavesplatform.matcher.market

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{Actor, ActorRef, Props, SupervisorStrategy, Terminated}
import com.google.common.base.Charsets
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.matcher.MatcherSettings
import com.wavesplatform.matcher.api.OrderBookUnavailable
import com.wavesplatform.matcher.db.AssetPairsDB
import com.wavesplatform.matcher.market.OrderBookActor._
import com.wavesplatform.matcher.queue.QueueEventWithMeta.{Offset => EventOffset}
import com.wavesplatform.matcher.queue.{QueueEvent, QueueEventWithMeta}
import com.wavesplatform.matcher.util.WorkingStash
import com.wavesplatform.state.AssetDescription
import com.wavesplatform.transaction.AssetId
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.ScorexLogging
import play.api.libs.json._
import scorex.utils._

class MatcherActor(settings: MatcherSettings,
                   assetPairsDB: AssetPairsDB,
                   recoveryCompletedWithEventNr: Either[String, (ActorRef, Long)] => Unit,
                   orderBooks: AtomicReference[Map[AssetPair, Either[Unit, ActorRef]]],
                   orderBookActorProps: (AssetPair, ActorRef) => Props,
                   assetDescription: ByteStr => Option[AssetDescription])
    extends Actor
    with WorkingStash
    with ScorexLogging {

  import MatcherActor._

  override def supervisorStrategy: SupervisorStrategy = SupervisorStrategy.stoppingStrategy

  private var tradedPairs: Map[AssetPair, MarketData] = Map.empty
  private var lastProcessedNr: Long                   = -1L

  private var snapshotsState = SnapshotsState.empty

  override val receive: Receive = {
    val knownAssetPairs = assetPairsDB.all()
    if (knownAssetPairs.isEmpty) {
      log.info("Recovery completed!")
      recoveryCompletedWithEventNr(Right((self, -1L)))
      working
    } else {
      log.info(s"Recovery completed, waiting order books to restore: ${knownAssetPairs.mkString(", ")}")
      knownAssetPairs.foreach(createOrderBook)
      collectOrderBooks(knownAssetPairs.size, None, -1L, Map.empty)
    }
  }

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

  private def createOrderBook(pair: AssetPair): ActorRef = {
    log.info(s"Creating order book for $pair")
    val orderBook = context.watch(context.actorOf(orderBookActorProps(pair, self), OrderBookActor.name(pair)))
    orderBooks.updateAndGet(_ + (pair -> Right(orderBook)))
    tradedPairs += pair -> createMarketData(pair)
    orderBook
  }

  /**
    * @param f (sender, orderBook)
    */
  private def runFor(assetPair: AssetPair, autoCreate: Boolean = true)(f: (ActorRef, ActorRef) => Unit): Unit = {
    val s = sender()
    orderBook(assetPair) match {
      case Some(Right(ob)) => f(s, ob)
      case Some(Left(_))   => s ! OrderBookUnavailable
      case None =>
        if (context.child(OrderBookActor.name(assetPair)).nonEmpty) {
          log.error(s"OrderBook for $assetPair is stopped, but it is not observed in orderBook")
          s ! OrderBookUnavailable
        } else if (autoCreate) {
          val ob = createOrderBook(assetPair)
          assetPairsDB.add(assetPair)
          f(s, ob)
        } else {
          log.warn(s"OrderBook for $assetPair is stopped and autoCreate is $autoCreate, respond to client with OrderBookUnavailable")
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

  private def working: Receive = {
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
            snapshotsState = snapshotsState.without(assetPair)
            tradedPairs -= assetPair
            assetPairsDB.remove(assetPair)
          }

        case _ => runFor(request.event.assetPair)((sender, orderBook) => orderBook.tell(request, sender))
      }
      lastProcessedNr = math.max(request.offset, lastProcessedNr)
      createSnapshotFor(lastProcessedNr)

    case request: ForceStartOrderBook =>
      runFor(request.assetPair)((sender, orderBook) => orderBook.tell(request, sender))

    case Shutdown =>
      context.children.foreach(context.unwatch)
      context.stop(self)

    case Terminated(ref) =>
      val name = ref.path.name
      val xs   = name.split("-")
      val pair = if (xs.size == 2) AssetPair.createAssetPair(xs.head, xs(1)).toOption else None

      pair.foreach { p =>
        orderBooks.getAndUpdate { obs =>
          obs.get(p) match {
            case None    => obs
            case Some(_) => obs.updated(p, Left(()))
          }
        }
      }

      if (pair.fold(true)(orderBooks.get.contains)) log.error(s"$ref is terminated")
      else log.info(s"$ref is terminated")

    case OrderBookRecovered(assetPair, eventNr) =>
      snapshotsState = snapshotsState.updated(assetPair, eventNr, lastProcessedNr, settings.snapshotsInterval)

    case OrderBookSnapshotUpdateCompleted(assetPair, currentOffset) =>
      snapshotsState = snapshotsState.updated(assetPair, currentOffset, lastProcessedNr, settings.snapshotsInterval)
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
      log.error(s"$ref is terminated during start, recovery failed")
      context.children.foreach(context.unwatch)
      context.stop(self)
      recoveryCompletedWithEventNr(Left(s"$ref is terminated"))

    case Shutdown =>
      context.children.foreach(context.unwatch)
      context.stop(self)
      recoveryCompletedWithEventNr(Left("Received Shutdown command"))

    case x => stash(x)
  }

  private def becomeWorking(oldestSnapshotOffset: Option[EventOffset],
                            newestSnapshotOffset: EventOffset,
                            currentOffsets: Map[AssetPair, Option[EventOffset]]): Unit = {
    context.become(working)

    // Imagine we have no order books and start the DEX:
    // index:     0  1  2  3  4  5  6  7  8  9 10 11 12
    // events:    A  A  B  C  A  B  A  A  A  A  B  B  A
    // snapshots:                   ^ for A           ^ for B
    // Then we restart the DEX:
    // 1. The DEX observes two snapshots: A (offset=6) and B (offset=12)
    // 2. The oldest snapshot is the snapshot for A with offset=6
    // 3. The DEX replays events from offset=6 and ignores offset=3 for order book C
    val safeStartOffset = oldestSnapshotOffset.fold(0L)(_ / settings.snapshotsInterval * settings.snapshotsInterval) - 1L

    val safestStartOffset = math.max(
      -1L,
      settings.limitEventsDuringRecovery.fold(safeStartOffset) { limitEventsDuringRecovery =>
        math.max(safeStartOffset, newestSnapshotOffset - limitEventsDuringRecovery)
      }
    )

    snapshotsState = SnapshotsState(
      currentOffsets = currentOffsets,
      lastProcessedOffset = newestSnapshotOffset,
      interval = settings.snapshotsInterval
    )

    log.info(
      s"All snapshots are loaded, oldestSnapshotOffset: $oldestSnapshotOffset, newestSnapshotOffset: $newestSnapshotOffset, safeStartOffset: $safeStartOffset, safestStartOffset: $safestStartOffset, newestSnapshotOffset: $newestSnapshotOffset")
    log.trace(s"Expecting snapshots at:\n${snapshotsState.nearestSnapshotOffsets.map { case (p, x) => s"$p -> $x" }.mkString("\n")}")

    unstashAll()
    recoveryCompletedWithEventNr(Right((self, safestStartOffset)))
  }
}

object MatcherActor {
  def name: String = "matcher"

  def props(matcherSettings: MatcherSettings,
            assetPairsDB: AssetPairsDB,
            recoveryCompletedWithEventNr: Either[String, (ActorRef, Long)] => Unit,
            orderBooks: AtomicReference[Map[AssetPair, Either[Unit, ActorRef]]],
            orderBookProps: (AssetPair, ActorRef) => Props,
            assetDescription: ByteStr => Option[AssetDescription]): Props =
    Props(
      new MatcherActor(
        matcherSettings,
        assetPairsDB,
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
