package com.wavesplatform.matcher

import java.io.File
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.pattern.{AskTimeoutException, gracefulStop}
import akka.stream.ActorMaterializer
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.CompositeHttpService
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.db._
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.matcher.Matcher.Status
import com.wavesplatform.matcher.api.{MatcherApiRoute, OrderBookSnapshotHttpCache}
import com.wavesplatform.matcher.history.HistoryRouter
import com.wavesplatform.matcher.market.OrderBookActor.MarketStatus
import com.wavesplatform.matcher.market.{ExchangeTransactionBroadcastActor, MatcherActor, MatcherTransactionWriter, OrderBookActor}
import com.wavesplatform.matcher.model.MatcherModel.Normalization
import com.wavesplatform.matcher.model.{ExchangeTransactionCreator, OrderBook, OrderValidator}
import com.wavesplatform.matcher.queue._
import com.wavesplatform.matcher.settings.MatcherSettings
import com.wavesplatform.state.VolumeAndFee
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.{ErrorStartingMatcher, ScorexLogging, forceStopApplication}
import net.ceedubs.ficus.Ficus._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success}

class Matcher(context: Context) extends Extension with ScorexLogging {

  private val settings = context.settings.config.as[MatcherSettings]("waves.matcher")

  private val matcherKeyPair = (for {
    address <- Address.fromString(settings.account)
    pk      <- context.wallet.privateKeyAccount(address)
  } yield pk).explicitGet()

  private def matcherPublicKey: PublicKey = matcherKeyPair

  private implicit val as: ActorSystem                 = context.actorSystem
  private implicit val materializer: ActorMaterializer = ActorMaterializer()
  import as.dispatcher

  private val status: AtomicReference[Status] = new AtomicReference(Status.Starting)
  private var currentOffset                   = -1L // Used only for REST API

  private val blacklistedAssets: Set[IssuedAsset] = settings.blacklistedAssets
    .map { assetName =>
      val asset = AssetPair
        .extractAssetId(assetName)
        .get

      asset match {
        case Waves              => throw new IllegalArgumentException("Can't blacklist the main coin")
        case a @ IssuedAsset(_) => a
      }
    }

  private val pairBuilder        = new AssetPairBuilder(settings, context.blockchain)
  private val orderBookCache     = new ConcurrentHashMap[AssetPair, OrderBook.AggregatedSnapshot](1000, 0.9f, 10)
  private val transactionCreator = new ExchangeTransactionCreator(context.blockchain, matcherKeyPair, settings)

  private val orderBooks = new AtomicReference(Map.empty[AssetPair, Either[Unit, ActorRef]])
  private val orderBooksSnapshotCache = new OrderBookSnapshotHttpCache(
    settings.orderBookSnapshotHttpCache,
    context.time,
    p => Option(orderBookCache.get(p))
  )

  private val marketStatuses = new ConcurrentHashMap[AssetPair, MarketStatus](1000, 0.9f, 10)

  private def updateOrderBookCache(assetPair: AssetPair)(newSnapshot: OrderBook.AggregatedSnapshot): Unit = {
    orderBookCache.put(assetPair, newSnapshot)
    orderBooksSnapshotCache.invalidate(assetPair)
  }

  private def orderBookProps(pair: AssetPair, matcherActor: ActorRef): Props = {

    val normalizedTickSize = settings.orderRestrictions.get(pair).withFilter(_.mergeSmallPrices).map { restrictions =>
      Normalization.normalizePrice(restrictions.tickSize, context.blockchain, pair).max(1)
    }

    OrderBookActor.props(
      matcherActor,
      addressActors,
      pair,
      updateOrderBookCache(pair),
      marketStatuses.put(pair, _),
      settings,
      transactionCreator.createTransaction,
      context.time,
      normalizedTickSize
    )
  }

  private val matcherQueue: MatcherQueue = settings.eventsQueue.tpe match {
    case "local" =>
      log.info("Events will be stored locally")
      new LocalMatcherQueue(settings.eventsQueue.local, new LocalQueueStore(db), context.time)

    case "kafka" =>
      log.info("Events will be stored in Kafka")
      new KafkaMatcherQueue(settings.eventsQueue.kafka)(materializer)

    case x => throw new IllegalArgumentException(s"Unknown queue type: $x")
  }

  private val getMarketStatus: AssetPair => Option[MarketStatus] = p => Option(marketStatuses.get(p))
  private val rateCache                                          = RateCache(db)

  private def validateOrder(o: Order) = {

    import com.wavesplatform.matcher.error._

    for {
      _ <- OrderValidator.matcherSettingsAware(matcherPublicKey, blacklistedAddresses, blacklistedAssets, settings, rateCache)(o)
      _ <- OrderValidator.timeAware(context.time)(o)
      _ <- OrderValidator.marketAware(settings.orderFee, settings.deviation, getMarketStatus(o.assetPair), rateCache)(o)
      _ <- OrderValidator.blockchainAware(
        context.blockchain,
        transactionCreator.createTransaction,
        matcherPublicKey.toAddress,
        context.time,
        settings.orderFee,
        settings.orderRestrictions,
        rateCache
      )(o)
      _ <- pairBuilder.validateAssetPair(o.assetPair).left.map(x => MatcherError.AssetPairCommonValidationFailed(o.assetPair, x))
    } yield o
  }

  lazy val matcherApiRoutes: Seq[MatcherApiRoute] = Seq(
    MatcherApiRoute(
      pairBuilder,
      matcherPublicKey,
      matcher,
      addressActors,
      matcherQueue.storeEvent,
      p => Option(orderBooks.get()).flatMap(_.get(p)),
      p => Option(marketStatuses.get(p)),
      validateOrder,
      orderBooksSnapshotCache,
      settings,
      () => status.get(),
      db,
      context.time,
      () => currentOffset,
      () => matcherQueue.lastEventOffset,
      ExchangeTransactionCreator.minAccountFee(context.blockchain, matcherPublicKey.toAddress),
      Base58.tryDecode(context.settings.config.getString("waves.rest-api.api-key-hash")).toOption,
      rateCache,
      settings.allowedOrderVersions.filter(OrderValidator.checkOrderVersion(_, context.blockchain).isRight)
    )
  )

  lazy val matcherApiTypes: Set[Class[_]] = Set(
    classOf[MatcherApiRoute]
  )

  private val snapshotsRestore = Promise[Unit]()

  lazy val matcher: ActorRef = context.actorSystem.actorOf(
    MatcherActor.props(
      settings, {
        case Left(msg) =>
          log.error(s"Can't start matcher: $msg")
          forceStopApplication(ErrorStartingMatcher)

        case Right((self, processedOffset)) =>
          currentOffset = processedOffset
          snapshotsRestore.trySuccess(())
          matcherQueue.startConsume(
            processedOffset + 1,
            eventWithMeta => {
              log.debug(s"Consumed $eventWithMeta")

              self ! eventWithMeta
              currentOffset = eventWithMeta.offset
            }
          )
      },
      orderBooks,
      orderBookProps,
      context.blockchain.assetDescription
    ),
    MatcherActor.name
  )

  private lazy val orderDb = OrderDB(settings, db)

  private lazy val historyRouter = settings.orderHistory.map { orderHistorySettings =>
    context.actorSystem.actorOf(HistoryRouter.props(context.blockchain, settings.postgresConnection, orderHistorySettings), "history-router")
  }

  private lazy val addressActors =
    context.actorSystem.actorOf(
      Props(
        new AddressDirectory(
          context.spendableBalanceChanged,
          settings,
          address =>
            Props(new AddressActor(
              address,
              context.utx.spendableBalance(address, _),
              5.seconds,
              context.time,
              orderDb,
              id => context.blockchain.filledVolumeAndFee(id) != VolumeAndFee.empty,
              matcherQueue.storeEvent
            )),
          historyRouter
        )),
      "addresses"
    )

  private lazy val blacklistedAddresses = settings.blacklistedAddresses.map(Address.fromString(_).explicitGet())

  private lazy val db = openDB(settings.dataDir)

  @volatile var matcherServerBinding: ServerBinding = _

  override def shutdown(): Future[Unit] = Future {
    log.info("Shutting down matcher")
    setStatus(Status.Stopping)

    Await.result(matcherServerBinding.unbind(), 10.seconds)

    val stopMatcherTimeout = 5.minutes
    matcherQueue.close(stopMatcherTimeout)

    orderBooksSnapshotCache.close()
    Await.result(gracefulStop(matcher, stopMatcherTimeout, MatcherActor.Shutdown), stopMatcherTimeout)
    materializer.shutdown()
    log.debug("Matcher's actor system has been shut down")
    db.close()
    log.debug("Matcher's database closed")
    log.info("Matcher shutdown successful")
  }

  private def checkDirectory(directory: File): Unit = if (!directory.exists()) {
    log.error(s"Failed to create directory '${directory.getPath}'")
    sys.exit(1)
  }

  override def start(): Unit = {
    val journalDir  = new File(settings.journalDataDir)
    val snapshotDir = new File(settings.snapshotsDataDir)
    journalDir.mkdirs()
    snapshotDir.mkdirs()

    checkDirectory(journalDir)
    checkDirectory(snapshotDir)

    log.info(s"Starting matcher on: ${settings.bindAddress}:${settings.port} ...")

    val combinedRoute = CompositeHttpService(matcherApiTypes, matcherApiRoutes, context.settings.restAPISettings).compositeRoute
    matcherServerBinding = Await.result(Http().bindAndHandle(combinedRoute, settings.bindAddress, settings.port), 5.seconds)

    log.info(s"Matcher bound to ${matcherServerBinding.localAddress}")
    context.actorSystem.actorOf(
      ExchangeTransactionBroadcastActor
        .props(
          settings.exchangeTransactionBroadcast,
          context.time,
          tx => context.utx.putIfNew(tx).resultE.isRight,
          context.blockchain.containsTransaction(_),
          txs => txs.foreach(context.broadcastTx)
        ),
      "exchange-transaction-broadcast"
    )

    context.actorSystem.actorOf(MatcherTransactionWriter.props(db, settings), MatcherTransactionWriter.name)

    val startGuard = for {
      _ <- waitSnapshotsRestored(settings.snapshotsLoadingTimeout)
      deadline = settings.startEventsProcessingTimeout.fromNow
      lastOffsetQueue <- getLastOffset(deadline)
      _ = log.info(s"Last queue offset is $lastOffsetQueue")
      _ <- waitOffsetReached(lastOffsetQueue, deadline)
    } yield ()

    startGuard.onComplete {
      case Success(_) => setStatus(Status.Working)
      case Failure(e) =>
        log.error(s"Can't start matcher: ${e.getMessage}", e)
        forceStopApplication(ErrorStartingMatcher)
    }
  }

  private def setStatus(newStatus: Status): Unit = {
    status.set(newStatus)
    log.info(s"Status now is $newStatus")
  }

  private def waitSnapshotsRestored(timeout: FiniteDuration): Future[Unit] = {
    val failure = Promise[Unit]()
    context.actorSystem.scheduler.scheduleOnce(timeout) {
      failure.failure(new TimeoutException(s"Can't restore snapshots in ${timeout.toSeconds} seconds"))
    }

    Future.firstCompletedOf[Unit](List(snapshotsRestore.future, failure.future))
  }

  private def getLastOffset(deadline: Deadline): Future[QueueEventWithMeta.Offset] = matcherQueue.lastEventOffset.recoverWith {
    case _: AskTimeoutException =>
      if (deadline.isOverdue()) Future.failed(new TimeoutException("Can't get last offset from queue"))
      else getLastOffset(deadline)
  }

  private def waitOffsetReached(lastQueueOffset: QueueEventWithMeta.Offset, deadline: Deadline): Future[Unit] = {
    val p = Promise[Unit]()

    def loop(): Unit = {
      if (currentOffset >= lastQueueOffset) p.trySuccess(())
      else if (deadline.isOverdue())
        p.tryFailure(new TimeoutException(s"Can't process all events in ${settings.startEventsProcessingTimeout.toMinutes} minutes"))
      else context.actorSystem.scheduler.scheduleOnce(1.second)(loop())
    }

    loop()
    p.future
  }
}

object Matcher extends ScorexLogging {
  type StoreEvent = QueueEvent => Future[Option[QueueEventWithMeta]]

  sealed trait Status
  object Status {
    case object Starting extends Status
    case object Working  extends Status
    case object Stopping extends Status
  }
}
