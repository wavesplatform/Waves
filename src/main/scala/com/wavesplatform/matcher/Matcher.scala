package com.wavesplatform.matcher

import java.io.File
import java.lang.{Long => jLong}
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ConcurrentHashMap, Executors, TimeUnit}

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.pattern.{ask, gracefulStop}
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.wavesplatform.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.api.http.CompositeHttpService
import com.wavesplatform.db._
import com.wavesplatform.matcher.api.{AlreadyProcessed, MatcherApiRoute, MatcherResponse, OrderBookSnapshotHttpCache}
import com.wavesplatform.matcher.market.OrderBookActor.MarketStatus
import com.wavesplatform.matcher.market.{MatcherActor, MatcherTransactionWriter, OrderBookActor}
import com.wavesplatform.matcher.model.{ExchangeTransactionCreator, OrderBook, OrderValidator}
import com.wavesplatform.matcher.queue._
import com.wavesplatform.network._
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{Blockchain, EitherExt2}
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order}
import com.wavesplatform.utils.{ScorexLogging, Time}
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.ChannelGroup

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class Matcher(actorSystem: ActorSystem,
              time: Time,
              utx: UtxPool,
              allChannels: ChannelGroup,
              blockchain: Blockchain,
              settings: WavesSettings,
              matcherPrivateKey: PrivateKeyAccount,
              isDuringShutdown: () => Boolean)
    extends ScorexLogging {

  import settings._

  private implicit val materializer: ActorMaterializer = ActorMaterializer()(actorSystem)

  private val currentOffset      = new AtomicReference[QueueEventWithMeta.Offset](-1L)
  private val pairBuilder        = new AssetPairBuilder(settings.matcherSettings, blockchain)
  private val orderBookCache     = new ConcurrentHashMap[AssetPair, OrderBook](1000, 0.9f, 10)
  private val transactionCreator = new ExchangeTransactionCreator(blockchain, matcherPrivateKey, matcherSettings)

  private val orderBooks = new AtomicReference(Map.empty[AssetPair, Either[Unit, ActorRef]])
  private val orderBooksSnapshotCache = new OrderBookSnapshotHttpCache(
    matcherSettings.orderBookSnapshotHttpCache,
    time,
    p => Option(orderBookCache.get(p))
  )

  private val marketStatuses = new ConcurrentHashMap[AssetPair, MarketStatus](1000, 0.9f, 10)

  private def updateOrderBookCache(assetPair: AssetPair)(newSnapshot: OrderBook): Unit = {
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

  private val requests: LoadingCache[jLong, Promise[MatcherResponse]] = CacheBuilder
    .newBuilder()
    .expireAfterWrite(6.seconds.toSeconds, TimeUnit.SECONDS)
    .build[jLong, Promise[MatcherResponse]](CacheLoader.from((_: jLong) => Promise[MatcherResponse]))

  private def validateOrder(o: Order) =
    for {
      _ <- OrderValidator.matcherSettingsAware(matcherPublicKey,
                                               blacklistedAddresses,
                                               matcherSettings.blacklistedAssets.map(AssetPair.extractAssetId(_).get))(o)
      _ <- OrderValidator.timeAware(time)(o)
      _ <- OrderValidator.blockchainAware(blockchain,
                                          transactionCreator.createTransaction,
                                          settings.matcherSettings.orderMatchTxFee,
                                          matcherPublicKey.toAddress,
                                          time)(o)
      _ <- pairBuilder.validateAssetPair(o.assetPair)
    } yield o

  private def storeEvent(event: QueueEvent): Future[MatcherResponse] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    matcherQueue.storeEvent(event).flatMap(offset => requests.get(jLong.valueOf(offset)).future)
  }

  lazy val matcherApiRoutes: Seq[MatcherApiRoute] = Seq(
    MatcherApiRoute(
      pairBuilder,
      matcherPublicKey,
      matcher,
      addressActors,
      storeEvent,
      p => Option(orderBooks.get()).flatMap(_.get(p)),
      p => Option(marketStatuses.get(p)),
      validateOrder,
      orderBooksSnapshotCache,
      settings,
      isDuringShutdown,
      db,
      time,
      () => currentOffset.get()
    )
  )

  lazy val matcherApiTypes: Set[Class[_]] = Set(
    classOf[MatcherApiRoute]
  )

  private val exchangeTxPool = ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  lazy val matcher: ActorRef = actorSystem.actorOf(
    MatcherActor.props(
      matcherSettings,
      (self, oldestSnapshotOffset, newestSnapshotOffset) => {
        import actorSystem.dispatcher
        implicit val timeout: Timeout = 5.seconds

        val ok = Success(())

        currentOffset.set(oldestSnapshotOffset)
        matcherQueue.startConsume(
          oldestSnapshotOffset + 1,
          eventWithMeta => {
            log.debug(s"[offset=${eventWithMeta.offset}, ts=${eventWithMeta.timestamp}] Consumed ${eventWithMeta.event}")

            // Ignoring possible timeouts or other errors
            // If an order book doesn't process a message, it will re-process all its messages after fix + restart
            self
              .ask(eventWithMeta)
              .mapTo[MatcherResponse]
              .map { r =>
                currentOffset.getAndAccumulate(eventWithMeta.offset, math.max(_, _))
                // We don't need to resolve old requests, those was did before restart, because we lost clients connections
                if (eventWithMeta.offset > newestSnapshotOffset) r match {
                  case AlreadyProcessed =>
                  case _                => requests.get(jLong.valueOf(eventWithMeta.offset)).trySuccess(r)
                }
              }
              .transform {
                case Failure(e) =>
                  log.warn(s"An error during processing an event with offset ${eventWithMeta.offset}: ${e.getMessage}", e)
                  ok
                case _ => ok
              }
          }
        )
      },
      orderBooks,
      orderBookProps,
      blockchain.assetDescription
    ),
    MatcherActor.name
  )

  private lazy val addressActors =
    actorSystem.actorOf(Props(new AddressDirectory(utx.portfolio, storeEvent, matcherSettings, OrderDB(matcherSettings, db))), "addresses")

  private lazy val blacklistedAddresses = settings.matcherSettings.blacklistedAddresses.map(Address.fromString(_).explicitGet())
  private lazy val matcherPublicKey     = PublicKeyAccount(matcherPrivateKey.publicKey)

  private lazy val db = openDB(matcherSettings.dataDir)

  @volatile var matcherServerBinding: ServerBinding = _

  def shutdown(): Unit = {
    log.info("Shutting down matcher")

    Await.result(matcherServerBinding.unbind(), 10.seconds)

    val stopMatcherTimeout = 5.minutes
    matcherQueue.close(stopMatcherTimeout)

    orderBooksSnapshotCache.close()
    Await.result(gracefulStop(matcher, stopMatcherTimeout, MatcherActor.Shutdown), stopMatcherTimeout)
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

    implicit val as: ActorSystem                 = actorSystem
    implicit val materializer: ActorMaterializer = ActorMaterializer()

    val combinedRoute = CompositeHttpService(actorSystem, matcherApiTypes, matcherApiRoutes, restAPISettings).compositeRoute
    matcherServerBinding = Await.result(Http().bindAndHandle(combinedRoute, matcherSettings.bindAddress, matcherSettings.port), 5.seconds)

    log.info(s"Matcher bound to ${matcherServerBinding.localAddress}")

    actorSystem.actorOf(MatcherTransactionWriter.props(db, matcherSettings), MatcherTransactionWriter.name)
  }
}

object Matcher extends ScorexLogging {
  type StoreEvent = QueueEvent => Future[MatcherResponse]

  def apply(actorSystem: ActorSystem,
            time: Time,
            wallet: Wallet,
            utx: UtxPool,
            allChannels: ChannelGroup,
            blockchain: Blockchain,
            settings: WavesSettings,
            isDuringShutdown: () => Boolean): Option[Matcher] =
    try {
      val privateKey = (for {
        address <- Address.fromString(settings.matcherSettings.account)
        pk      <- wallet.privateKeyAccount(address)
      } yield pk).explicitGet()

      val matcher = new Matcher(actorSystem, time, utx, allChannels, blockchain, settings, privateKey, isDuringShutdown)
      matcher.runMatcher()
      Some(matcher)
    } catch {
      case NonFatal(e) =>
        log.warn("Error starting matcher", e)
        None
    }
}
