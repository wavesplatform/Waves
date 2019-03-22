package com.wavesplatform.matcher

import java.io.File
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ConcurrentHashMap, Executors, TimeoutException}

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.pattern.{AskTimeoutException, gracefulStop}
import akka.stream.ActorMaterializer
import com.wavesplatform.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.api.http.CompositeHttpService
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db._
import com.wavesplatform.matcher.Matcher.Status
import com.wavesplatform.matcher.api.{MatcherApiRoute, OrderBookSnapshotHttpCache}
import com.wavesplatform.matcher.market.OrderBookActor.MarketStatus
import com.wavesplatform.matcher.market.{MatcherActor, MatcherTransactionWriter, OrderBookActor}
import com.wavesplatform.matcher.model.{ExchangeTransactionCreator, OrderBook, OrderValidator}
import com.wavesplatform.matcher.queue._
import com.wavesplatform.network._
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{Blockchain, VolumeAndFee}
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.{ErrorStartingMatcher, ScorexLogging, Time, forceStopApplication}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import monix.reactive.Observable

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class Matcher(actorSystem: ActorSystem,
              time: Time,
              utx: UtxPool,
              allChannels: ChannelGroup,
              blockchain: Blockchain,
              spendableBalanceChanged: Observable[(Address, Asset)],
              settings: WavesSettings,
              matcherPrivateKey: PrivateKeyAccount)
    extends ScorexLogging {

  import settings._

  private implicit val as: ActorSystem                 = actorSystem
  private implicit val materializer: ActorMaterializer = ActorMaterializer()
  import as.dispatcher

  private val status: AtomicReference[Status] = new AtomicReference(Status.Starting)
  private var currentOffset                   = -1L // Used only for REST API

  private val blacklistedAssets: Set[IssuedAsset] = matcherSettings.blacklistedAssets
    .map { assetName =>
      val asset = AssetPair
        .extractAssetId(assetName)
        .get

      asset match {
        case Waves              => throw new IllegalArgumentException("Can't blacklist the main coin")
        case a @ IssuedAsset(_) => a
      }
    }

  private val pairBuilder        = new AssetPairBuilder(settings.matcherSettings, blockchain)
  private val orderBookCache     = new ConcurrentHashMap[AssetPair, OrderBook.AggregatedSnapshot](1000, 0.9f, 10)
  private val transactionCreator = new ExchangeTransactionCreator(blockchain, matcherPrivateKey, matcherSettings.orderFee)

  private val orderBooks = new AtomicReference(Map.empty[AssetPair, Either[Unit, ActorRef]])
  private val orderBooksSnapshotCache = new OrderBookSnapshotHttpCache(
    matcherSettings.orderBookSnapshotHttpCache,
    time,
    p => Option(orderBookCache.get(p))
  )

  private val marketStatuses = new ConcurrentHashMap[AssetPair, MarketStatus](1000, 0.9f, 10)

  private def updateOrderBookCache(assetPair: AssetPair)(newSnapshot: OrderBook.AggregatedSnapshot): Unit = {
    orderBookCache.put(assetPair, newSnapshot)
    orderBooksSnapshotCache.invalidate(assetPair)
  }

  private def orderBookProps(pair: AssetPair, matcherActor: ActorRef): Props = OrderBookActor.props(
    matcherActor,
    addressActors,
    pair,
    updateOrderBookCache(pair),
    marketStatuses.put(pair, _),
    tx => exchangeTxPool.execute(() => if (utx.putIfNew(tx).isRight) allChannels.broadcastTx(tx)),
    matcherSettings,
    transactionCreator.createTransaction,
    time
  )

  private val matcherQueue: MatcherQueue = settings.matcherSettings.eventsQueue.tpe match {
    case "local" =>
      log.info("Events will be stored locally")
      new LocalMatcherQueue(settings.matcherSettings.eventsQueue.local, new LocalQueueStore(db), time)(actorSystem.dispatcher)

    case "kafka" =>
      log.info("Events will be stored in Kafka")
      new KafkaMatcherQueue(settings.matcherSettings.eventsQueue.kafka)(materializer)

    case x => throw new IllegalArgumentException(s"Unknown queue type: $x")
  }

  private val getMarketStatus: AssetPair => Option[MarketStatus] = p => Option(marketStatuses.get(p))

  private def validateOrder(o: Order) = {
    import com.wavesplatform.matcher.error._
    for {
      _ <- OrderValidator.matcherSettingsAware(matcherPublicKey, blacklistedAddresses, blacklistedAssets, matcherSettings)(o)
      _ <- OrderValidator.timeAware(time)(o)
      _ <- OrderValidator.marketAware(matcherSettings.orderFee, matcherSettings.deviation, getMarketStatus(o.assetPair))(o)
      _ <- OrderValidator.blockchainAware(blockchain,
                                          transactionCreator.createTransaction,
                                          matcherPublicKey.toAddress,
                                          time,
                                          matcherSettings.orderFee)(o)
      _ <- pairBuilder.validateAssetPair(o.assetPair).left.map(x => MatcherError.AssetPairCommonValidationFailed(x))
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
      getMarketStatus,
      validateOrder,
      orderBooksSnapshotCache,
      settings,
      () => status.get(),
      db,
      time,
      () => currentOffset,
      ExchangeTransactionCreator.minAccountFee(blockchain, matcherPublicKey.toAddress)
    )
  )

  lazy val matcherApiTypes: Set[Class[_]] = Set(
    classOf[MatcherApiRoute]
  )

  private val exchangeTxPool = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  private val snapshotsRestore = Promise[Unit]()

  lazy val matcher: ActorRef = actorSystem.actorOf(
    MatcherActor.props(
      matcherSettings, {
        case Left(msg) =>
          log.error(s"Can't start matcher: $msg")
          forceStopApplication(ErrorStartingMatcher)

        case Right((self, oldestSnapshotOffset)) =>
          currentOffset = oldestSnapshotOffset
          snapshotsRestore.trySuccess(())
          matcherQueue.startConsume(
            oldestSnapshotOffset + 1,
            eventWithMeta => {
              log.debug(s"[offset=${eventWithMeta.offset}, ts=${eventWithMeta.timestamp}] Consumed ${eventWithMeta.event}")

              self ! eventWithMeta
              currentOffset = eventWithMeta.offset
            }
          )
      },
      orderBooks,
      orderBookProps,
      blockchain.assetDescription
    ),
    MatcherActor.name
  )

  private lazy val orderDb = OrderDB(matcherSettings, db)

  private lazy val addressActors =
    actorSystem.actorOf(
      Props(
        new AddressDirectory(
          spendableBalanceChanged,
          matcherSettings,
          address =>
            Props(
              new AddressActor(
                address,
                utx.spendableBalance(address, _),
                5.seconds,
                time,
                orderDb,
                id => blockchain.filledVolumeAndFee(id) != VolumeAndFee.empty,
                matcherQueue.storeEvent
              ))
        )),
      "addresses"
    )

  private lazy val blacklistedAddresses = settings.matcherSettings.blacklistedAddresses.map(Address.fromString(_).explicitGet())
  private lazy val matcherPublicKey     = PublicKeyAccount(matcherPrivateKey.publicKey)

  private lazy val db = openDB(matcherSettings.dataDir)

  @volatile var matcherServerBinding: ServerBinding = _

  def shutdown(): Unit = {
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

  def runMatcher(): Unit = {
    val journalDir  = new File(matcherSettings.journalDataDir)
    val snapshotDir = new File(matcherSettings.snapshotsDataDir)
    journalDir.mkdirs()
    snapshotDir.mkdirs()

    checkDirectory(journalDir)
    checkDirectory(snapshotDir)

    log.info(s"Starting matcher on: ${matcherSettings.bindAddress}:${matcherSettings.port} ...")

    val combinedRoute = CompositeHttpService(matcherApiTypes, matcherApiRoutes, restAPISettings).compositeRoute
    matcherServerBinding = Await.result(Http().bindAndHandle(combinedRoute, matcherSettings.bindAddress, matcherSettings.port), 5.seconds)

    log.info(s"Matcher bound to ${matcherServerBinding.localAddress}")

    actorSystem.actorOf(MatcherTransactionWriter.props(db, matcherSettings), MatcherTransactionWriter.name)

    val startGuard = for {
      _ <- waitSnapshotsRestored(settings.matcherSettings.snapshotsLoadingTimeout)
      deadline = settings.matcherSettings.startEventsProcessingTimeout.fromNow
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
    actorSystem.scheduler.scheduleOnce(timeout) {
      failure.failure(new TimeoutException("Can't restore snapshots in time"))
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
      else if (deadline.isOverdue()) p.tryFailure(new TimeoutException("Can't process all events in time"))
      else actorSystem.scheduler.scheduleOnce(1.second)(loop())
    }

    loop()
    p.future
  }
}

object Matcher extends ScorexLogging {
  type StoreEvent = QueueEvent => Future[QueueEventWithMeta]

  def apply(actorSystem: ActorSystem,
            time: Time,
            wallet: Wallet,
            utx: UtxPool,
            allChannels: ChannelGroup,
            blockchain: Blockchain,
            spendableBalanceChanged: Observable[(Address, Asset)],
            settings: WavesSettings): Option[Matcher] =
    try {
      val privateKey = (for {
        address <- Address.fromString(settings.matcherSettings.account)
        pk      <- wallet.privateKeyAccount(address)
      } yield pk).explicitGet()

      val matcher = new Matcher(actorSystem, time, utx, allChannels, blockchain, spendableBalanceChanged, settings, privateKey)
      matcher.runMatcher()
      Some(matcher)
    } catch {
      case NonFatal(e) =>
        log.warn("Error starting matcher", e)
        forceStopApplication(ErrorStartingMatcher)
        None
    }

  sealed trait Status
  object Status {
    case object Starting extends Status
    case object Working  extends Status
    case object Stopping extends Status
  }
}
