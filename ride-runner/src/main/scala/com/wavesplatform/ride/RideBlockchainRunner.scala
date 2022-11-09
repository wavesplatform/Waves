package com.wavesplatform.ride

import cats.syntax.option.*
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.Application
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.blockchain.caches.LevelDbPersistentCaches
import com.wavesplatform.blockchain.storage.{AppendResult, RollbackResult}
import com.wavesplatform.blockchain.{BlockchainState, DataKey, RideBlockchain, SharedBlockchainStorage}
import com.wavesplatform.database.openDB
import com.wavesplatform.events.api.grpc.protobuf.SubscribeEvent
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.grpc.BlockchainGrpcApi.Event
import com.wavesplatform.grpc.{BlockchainGrpcApi, GrpcClientSettings, GrpcConnector}
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.resources.*
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
import scala.util.chaining.*
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
      val dbCaches          = new LevelDbPersistentCaches(db)
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

      val end               = lastHeightAtStart + 5
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
        .filter(_.nonEmpty)
        .foreach { batchedEvents =>
          val processResult = batchedEvents.foldLeft(ProcessResult()) { case (r, event) =>
            log.info(s"Processing ${event.getUpdate.height}")
            process(blockchainStorage, allScriptIndices, r, event)
          }

          val h = processResult.newHeight
          if (processResult.uncertainKeys.nonEmpty) {
            log.debug(s"Getting data for keys: ${processResult.uncertainKeys.toVector.map(_.toString).sorted.mkString(", ")}")
            processResult.uncertainKeys.foreach { _.reload(blockchainStorage, h) }
          }

          log.info(
            s"==> $h >= $lastHeightAtStart, started: $started, affectedScripts: ${processResult.affectedScripts}, batchedEvents for heights: {${batchedEvents
              .map(_.getUpdate.height)
              .mkString(", ")}}"
          )
          if (h >= lastHeightAtStart) {
            if (!started) {
              log.debug(s"[$h] Reached the current height, run all scripts")
              runScripts(h, allScriptIndices)
              started = true
            } else if (processResult.affectedScripts.isEmpty) {
              log.debug(s"[$h] Not updated")
            } else {
              log.debug(s"[$h] Updated for: ${processResult.affectedScripts.mkString(", ")}")
              runScripts(h, processResult.affectedScripts)
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

  // TODO don't calculate affectedScripts if all scripts are affected
  private case class ProcessResult(
      /*allScripts: Set[Int], */ newHeight: Int = 0,
      affectedScripts: Set[Int] = Set.empty,
      uncertainKeys: Set[DataKey] = Set.empty
  ) {
    def withAppendResult(x: AppendResult[Int]): ProcessResult =
      copy(
        affectedScripts = affectedScripts ++ x.affectedTags,
        uncertainKeys = x.mayBeChangedKey.foldLeft(uncertainKeys)(_ - _)
      )

    def withRollbackResult(x: RollbackResult[Int]): ProcessResult =
      copy(
        affectedScripts = affectedScripts ++ x.affectedTags,
        uncertainKeys = uncertainKeys ++ x.mayBeUncertainKey
      )
  }

  private def process(
      blockchainStorage: SharedBlockchainStorage[Int],
      allScriptIndices: Set[Int],
      prev: ProcessResult,
      event: SubscribeEvent
  ): ProcessResult = {
    val update = event.getUpdate
    val h      = update.height

    // TODO the height will be eventually > if this is a rollback
    // Almost all scripts use the height
    val withUpdatedHeight = if (h != blockchainStorage.height) {
      blockchainStorage.setHeight(h)
      prev.copy(affectedScripts = allScriptIndices, newHeight = h)
    } else prev.copy(newHeight = h)

    update.update match {
      case Update.Empty => prev
      case Update.Append(append) =>
        val txs = append.body match {
          case Body.Block(block)           => block.getBlock.transactions
          case Body.MicroBlock(microBlock) => microBlock.getMicroBlock.getMicroBlock.transactions
          case Body.Empty                  => Seq.empty
        }

        val stateUpdate = (append.getStateUpdate +: append.transactionStateUpdates).view
        withUpdatedHeight
          .pipe(stateUpdate.flatMap(_.assets).foldLeft(_) { case (r, x) =>
            r.withAppendResult(blockchainStorage.assets.append(h, x))
          })
          .pipe(stateUpdate.flatMap(_.balances).foldLeft(_) { case (r, x) =>
            r.withAppendResult(blockchainStorage.portfolios.append(h, x))
          })
          .pipe(stateUpdate.flatMap(_.leasingForAddress).foldLeft(_) { case (r, x) =>
            r.withAppendResult(blockchainStorage.portfolios.append(h, x))
          })
          .pipe(stateUpdate.flatMap(_.dataEntries).foldLeft(_) { case (r, x) =>
            r.withAppendResult(blockchainStorage.data.append(h, x))
          })
          .pipe(
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
              .foldLeft(_) { case (r, (pk, script)) =>
                r.withAppendResult(blockchainStorage.accountScripts.append(h, pk, script))
              }
          )
          .pipe(append.transactionIds.view.zip(txs).foldLeft(_) { case (r, (txId, tx)) =>
            r.withAppendResult(blockchainStorage.transactions.append(h, txId, tx))
          })

      case Update.Rollback(rollback) =>
        val stateUpdate = rollback.getRollbackStateUpdate
        withUpdatedHeight
          .pipe(stateUpdate.assets.foldLeft(_) { case (r, x) =>
            r.withRollbackResult(blockchainStorage.assets.rollback(h, x))
          })
          .pipe(stateUpdate.balances.foldLeft(_) { case (r, x) =>
            r.withRollbackResult(blockchainStorage.portfolios.rollback(h, x))
          })
          .pipe(stateUpdate.leasingForAddress.foldLeft(_) { case (r, x) =>
            r.withRollbackResult(blockchainStorage.portfolios.rollback(h, x))
          })
          .pipe(stateUpdate.dataEntries.foldLeft(_) { case (r, x) =>
            r.withRollbackResult(blockchainStorage.data.rollback(h, x))
          })
      /* TODO:
        .pipe(stateUpdate.accountScripts.foldLeft(_) { case (r, x) =>
          r.withRollbackResult(blockchainStorage.accountScripts.rollback(h, x))
        })
        // TODO Remove?
        .pipe(append.transactionIds.view.zip(txs).foldLeft(_) { case (r, txId) =>
          r.withRollbackResult(blockchainStorage.transactions.rollback(h, txId)
        })
       */
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
