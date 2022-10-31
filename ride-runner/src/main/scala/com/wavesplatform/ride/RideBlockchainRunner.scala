package com.wavesplatform.ride

import cats.implicits.*
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.Application
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.database.openDB
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.grpc.BlockchainGrpcApi.Event
import com.wavesplatform.grpc.{BlockchainGrpcApi, GrpcClientSettings, GrpcConnector}
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.resources.*
import com.wavesplatform.ride.blockchain.caches.LevelDbBlockchainCaches
import com.wavesplatform.ride.blockchain.{RideBlockchain, SharedBlockchainStorage}
import com.wavesplatform.ride.input.RunnerRequest
import com.wavesplatform.state.Blockchain
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

      val start             = blockchainStorage.height - 2
      val lastHeightAtStart = blockchainApi.getCurrentBlockchainHeight()
      log.info(s"Current blockchain height: $lastHeightAtStart")

      val end               = lastHeightAtStart + 1
      @volatile var started = false

      val events = blockchainApi.stream
        .takeWhile {
          case Event.Closed      => false
          case Event.Next(event) => event.getUpdate.height < end
          case _                 => false
        }
        .foreach {
          case Event.Failed(error) => println(error)
          case Event.Closed        => println("Closed")
          case Event.Next(event) =>
            val update = event.getUpdate
            update.update match {
              case Update.Append(append) =>
                val txs = append.body match {
                  case Body.Block(block)           => block.getBlock.transactions
                  case Body.MicroBlock(microBlock) => microBlock.getMicroBlock.getMicroBlock.transactions
                  case Body.Empty                  => Seq.empty
                }

//                log.info(
//                  s"${update.height}: assets=${stateUpdate.assets.size}, balances=${stateUpdate.balances.size}, " +
//                    s"leasingForAddress=${stateUpdate.leasingForAddress.size}, dataEntries=${stateUpdate.dataEntries.size}, " +
//                    s"dataUpdates=${dataUpdates.size}: ${dataUpdates.map(x => s"${x.key} -> ${x.value}").mkString(", ")}"
//                )

                // Almost all scripts use the height
                val updatedByHeight = if (update.height > blockchainStorage.height) {
                  blockchainStorage.setHeight(update.height)
                  allScriptIndices
                } else Set.empty[Int]

                val stateUpdate = (append.getStateUpdate +: append.transactionStateUpdates).view
                val updated = updatedByHeight union
                  stateUpdate.flatMap(_.assets).map(_.getAfter).foldLeft(Set.empty[Int]) { case (r, x) =>
                    r union blockchainStorage.replaceAssetDescription(x)
                  } union
                  stateUpdate.flatMap(_.balances).foldLeft(Set.empty[Int]) { case (r, x) =>
                    r union blockchainStorage.replaceBalance(x)
                  } union
                  stateUpdate.flatMap(_.leasingForAddress).foldLeft(Set.empty[Int]) {
                    _ union blockchainStorage.replaceLeasing(_)
                  } union
                  stateUpdate.flatMap(_.dataEntries).foldLeft(Set.empty[Int]) { case (r, x) =>
                    r union blockchainStorage.replaceAccountData(x)
                  } union
                  txs.view
                    .map(_.transaction)
                    .flatMap {
                      case Transaction.WavesTransaction(tx) =>
                        tx.data match {
                          case Data.SetScript(txData) => (tx.senderPublicKey.toPublicKey, txData.script).some
                          case _                      => none
                        }
                      case _ => none
                    }
                    .foldLeft(Set.empty[Int]) { case (r, (pk, script)) =>
                      r union blockchainStorage.replaceAccountScript(pk, script)
                    } union
                  append.transactionIds.foldLeft(Set.empty[Int]) { case (r, x) =>
                    r union blockchainStorage.replaceTransactionMeta(x, update.height)
                  }

                if (update.height >= lastHeightAtStart) {
                  if (!started) {
                    log.debug(s"[${update.height}] Reached the current height, run all scripts")
                    runScripts(update.height, allScriptIndices)
                    started = true
                  } else if (updated.isEmpty) {
                    log.debug(s"[${update.height}] Not updated")
                  } else {
                    log.debug(s"[${update.height}] Updated for: ${updated.mkString(", ")}")
                    runScripts(update.height, updated)
                  }
                }

              case _: Update.Rollback => log.info("Rollback, ignore")
              case Update.Empty       =>
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
