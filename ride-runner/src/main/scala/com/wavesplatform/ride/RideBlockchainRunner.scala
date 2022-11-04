package com.wavesplatform.ride

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.Application
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.database.openDB
import com.wavesplatform.grpc.BlockchainGrpcApi.Event
import com.wavesplatform.grpc.{BlockchainGrpcApi, GrpcClientSettings, GrpcConnector}
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.resources.*
import com.wavesplatform.ride.blockchain.caches.LevelDbBlockchainCaches
import com.wavesplatform.ride.blockchain.{BlockchainState, BlockchainUpdatedDiff, RideBlockchain, SharedBlockchainStorage}
import com.wavesplatform.ride.input.RunnerRequest
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.utils.ScorexLogging
import monix.execution.Scheduler
import play.api.libs.json.JsObject

import java.io.File
import java.util.concurrent.Executors
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}
import scala.io.Source
import scala.util.{Failure, Using}

object RideBlockchainRunner extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val basePath     = args(0)
    val nodeSettings = Application.loadApplicationConfig(Some(new File(s"$basePath/node/waves.conf")))

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = 'W'.toByte
    }

    // TODO expr should work too
    log.info("Loading args...")
    val input = RideRunnerInput.parseMany(Using(Source.fromFile(new File(s"$basePath/input4.json")))(_.getLines().mkString("\n")).get)

    val r = Using.Manager { use =>
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
          2,
          new ThreadFactoryBuilder().setNameFormat("common-scheduler-%d").setDaemon(false).build()
        )
      )

      val blockchainApi = use(
        new BlockchainGrpcApi(
          settings = BlockchainGrpcApi.Settings(1.minute),
          grpcApiChannel = grpcApiChannel,
          hangScheduler = commonScheduler
        )
      )

      val db                = use(openDB(s"$basePath/db"))
      val dbCaches          = new LevelDbBlockchainCaches(db)
      val blockchainStorage = new SharedBlockchainStorage[Int](nodeSettings.blockchainSettings, dbCaches, blockchainApi)

      val scripts          = input.zipWithIndex.map { case (input, index) => RideScript(index, blockchainStorage, input.request) }
      val allScriptIndices = scripts.indices.toSet

      def runScripts(height: Int, updated: Set[Int]): Unit = {
        updated.foreach { index =>
          val apiResult = scripts(index).run()
          log.info(s"[$height, $index] apiResult: ${apiResult.value("result").as[JsObject].value("value")}")
        }
      }

      log.info("Warm up caches...") // Also helps to figure out, which data is used by a script
      runScripts(blockchainStorage.height, allScriptIndices)

      val start             = Height(blockchainStorage.height - 2)
      val lastHeightAtStart = blockchainApi.getCurrentBlockchainHeight()
      log.info(s"Current blockchain height: $lastHeightAtStart")

      val end               = lastHeightAtStart + 1
      @volatile var started = false

      val events = blockchainApi.stream
        .takeWhile {
          case Event.Next(event) => event.getUpdate.height < end
          case Event.Closed =>
            log.info("Blockchain stream closed")
            false
          case Event.Failed(error) =>
            log.error("Blockchain stream failed", error)
            false
        }
        .collect {
          // TODO
          case Event.Next(event) => event
        }
        .mapAccumulate(BlockchainState.Working(start): BlockchainState)(BlockchainState.apply)
        .foreach { batchedEvents =>
          val diff = batchedEvents.map(_.getUpdate).foldLeft(BlockchainUpdatedDiff())(BlockchainUpdatedDiff.append)
          val h    = diff.newHeight

          // Almost all scripts use the height
          val updatedByHeight = if (h > blockchainStorage.height) {
            blockchainStorage.setHeight(h)
            allScriptIndices
          } else Set.empty[Int]

          val updated = updatedByHeight union
            diff.assetDetails.values.foldLeft(Set.empty[Int]) { case (r, update) =>
              r union blockchainStorage.setAssetDescription(update.height, update.value.getAfter)
            } union
            diff.balances.values.foldLeft(Set.empty[Int]) { case (r, update) =>
              r union blockchainStorage.setBalance(update.height, update.value)
            } union
            diff.leasingForAddress.values.foldLeft(Set.empty[Int]) { case (r, update) =>
              r union blockchainStorage.replaceLeasing(update.height, update.value)
            } union
            diff.dataEntries.values.foldLeft(Set.empty[Int]) { case (r, update) =>
              r union blockchainStorage.setAccountData(update.height, update.value)
            } union
            diff.updatedAccountScriptsByPk.foldLeft(Set.empty[Int]) { case (r, (pk, script)) =>
              r union blockchainStorage.setAccountScript(script.height, pk.toPublicKey, script.value)
            } union
            diff.newTransactionIds.foldLeft(Set.empty[Int]) { case (r, update) =>
              r union blockchainStorage.setTransactionMeta(update.height, update.value)
            } // TODO removedTransactionIds

          if (h >= lastHeightAtStart) {
            if (!started) {
              log.debug(s"[$h] Reached the current height, run all scripts")
              runScripts(h, allScriptIndices)
              started = true
            } else if (updated.isEmpty) {
              log.debug(s"[$h] Not updated")
            } else {
              log.debug(s"[$h] Updated for: ${updated.mkString(", ")}")
              runScripts(h, updated)
            }
          }
        }(Scheduler(commonScheduler))

      log.info(s"Watching blockchain updates from $start...")
      blockchainApi.watchBlockchainUpdates(blockchainUpdatesApiChannel, start)

      Await.result(events, Duration.Inf)
    }

    r match {
      case Failure(e) => log.error("Got an error", e)
      case _          => log.info("Done")
    }
  }
}

class RideScript(val index: Int, blockchain: Blockchain, runnerRequest: RunnerRequest) {
  def run(): JsObject = executeUtilsEvaluate(
    blockchain,
    runnerRequest
  )
}

object RideScript {
  def apply(index: Int, blockchainStorage: SharedBlockchainStorage[Int], runnerRequest: RunnerRequest): RideScript =
    new RideScript(index, new RideBlockchain[Int](blockchainStorage, index), runnerRequest)
}
