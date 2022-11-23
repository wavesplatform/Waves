package com.wavesplatform.ride.app

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.blockchain.BlockchainProcessor.RequestKey
import com.wavesplatform.blockchain.{BlockchainProcessor, BlockchainState, SharedBlockchainData}
import com.wavesplatform.database.openDB
import com.wavesplatform.grpc.BlockchainApi.Event
import com.wavesplatform.grpc.{DefaultBlockchainApi, GrpcClientSettings, GrpcConnector}
import com.wavesplatform.resources.*
import com.wavesplatform.state.Height
import com.wavesplatform.storage.persistent.LevelDbPersistentCaches
import com.wavesplatform.utils.ScorexLogging
import monix.eval.Task
import monix.execution.{ExecutionModel, Scheduler}
import play.api.libs.json.Json
import sttp.client3.HttpURLConnectionBackend

import java.io.File
import java.util.concurrent.Executors
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}
import scala.io.Source
import scala.util.Failure

object RideWithBlockchainUpdatesApp extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val startTs       = System.nanoTime()
    val basePath      = args(0)
    val (_, settings) = AppInitializer.init(Some(new File(s"$basePath/node/waves.conf")))

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = 'W'.toByte
    }

    val r = Using.Manager { use =>
      log.info("Loading args...")
      val scripts = Json
        .parse(use(Source.fromFile(new File(s"$basePath/input5.json"))).getLines().mkString("\n"))
        .as[List[RequestKey]]

      val connector = use(new GrpcConnector)

      log.info("Making gRPC channel to gRPC API...")
      val grpcApiChannel = use(
        connector.mkChannel(
          GrpcClientSettings(
            target = "grpc.wavesnodes.com:6870",
            maxHedgedAttempts = 5,
            maxRetryAttempts = 30,
            keepAliveWithoutCalls = false,
            keepAliveTime = 60.seconds,
            keepAliveTimeout = 15.seconds,
            idleTimeout = 300.days,
            maxInboundMessageSize = 8388608, // 8 MiB
            channelOptions = GrpcClientSettings.ChannelOptionsSettings(
              connectTimeout = 5.seconds
            )
          )
        )
      )

      log.info("Making gRPC channel to Blockchain Updates API...")
      val blockchainUpdatesApiChannel = use(
        connector.mkChannel(
          GrpcClientSettings(
            target = "grpc.wavesnodes.com:6881",
            maxHedgedAttempts = 5,
            maxRetryAttempts = 30,
            keepAliveWithoutCalls = false,
            keepAliveTime = 60.seconds,
            keepAliveTimeout = 15.seconds,
            idleTimeout = 300.days,
            maxInboundMessageSize = 8388608, // 8 MiB
            channelOptions = GrpcClientSettings.ChannelOptionsSettings(
              connectTimeout = 5.seconds
            )
          )
        )
      )

      val commonScheduler = use(
        Executors.newScheduledThreadPool(
          8,
          new ThreadFactoryBuilder().setNameFormat("common-scheduler-%d").setDaemon(false).build()
        )
      )

      val httpBackend = use.acquireWithShutdown(HttpURLConnectionBackend())(_.close())

      val blockchainApi = new DefaultBlockchainApi(
        settings = settings.rideRunner.blockchainApi,
        grpcApiChannel = grpcApiChannel,
        blockchainUpdatesApiChannel = blockchainUpdatesApiChannel,
        httpBackend = httpBackend,
        hangScheduler = commonScheduler
      )

      val db                = use(openDB(s"$basePath/db"))
      val dbCaches          = new LevelDbPersistentCaches(db)
      val blockchainStorage = new SharedBlockchainData[RequestKey](settings.blockchain, dbCaches, blockchainApi)

      val lastHeightAtStart = Height(blockchainApi.getCurrentBlockchainHeight())
      log.info(s"Current height: $lastHeightAtStart")

      val processor = BlockchainProcessor.mk(
        settings.rideRunner.processor,
        use.acquireWithShutdown(Scheduler(commonScheduler).withExecutionModel(ExecutionModel.AlwaysAsyncExecution))(_.shutdown()), // TODO
        blockchainStorage,
        scripts
      )

      log.info("Warm up caches...") // Also helps to figure out, which data is used by a script
      processor.runScripts(forceAll = true)

      val start = Height(3393500)     // math.max(0, blockchainStorage.height - 100 - 1))
      val end   = Height(start + 101) // lastHeightAtStart

      val blockchainUpdates = use(blockchainApi.mkBlockchainUpdatesStream())
      val events = blockchainUpdates.stream
        .doOnError(e =>
          Task {
            log.error("Error!", e)
          }
        )
        .takeWhile {
          case _: Event.Next => true
          case Event.Closed =>
            log.info("Blockchain stream closed")
            false
          case Event.Failed(error) =>
            log.error("Blockchain stream failed", error)
            false
        }
        .collect { case Event.Next(event) => event }
        .foldLeftL(BlockchainState.Starting(end): BlockchainState)(BlockchainState(processor, _, _))
        .runToFuture(Scheduler(commonScheduler))

      log.info(s"Watching blockchain updates...")

      blockchainUpdates.start(start + 1, end) // TODO end

      Await.result(events, Duration.Inf)
    }

    val duration = System.nanoTime() - startTs
    r match {
      case Failure(e) => log.error("Got an error", e)
      case _          => log.info(f"Done in ${duration / 1e9d}%5f s")
    }
  }
}
