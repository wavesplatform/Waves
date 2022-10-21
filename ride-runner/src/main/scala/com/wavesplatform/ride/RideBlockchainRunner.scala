package com.wavesplatform.ride

import cats.implicits.*
import scalapb.json4s.JsonFormat
import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.Application
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.events.protobuf.BlockchainUpdated.Append.Body
import com.wavesplatform.events.protobuf.BlockchainUpdated.Update
import com.wavesplatform.grpc.BlockchainGrpcApi.Event
import com.wavesplatform.grpc.{BlockchainGrpcApi, GrpcClientSettings, GrpcConnector}
import com.wavesplatform.protobuf.ByteStringExt
import com.wavesplatform.protobuf.transaction.SignedTransaction.Transaction
import com.wavesplatform.protobuf.transaction.Transaction.Data
import com.wavesplatform.resources.*
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
    val input = RideRunnerInput.parse(Using(Source.fromFile(new File(s"$basePath/input2.json")))(_.getLines().mkString("\n")).get)
    val r = Using.Manager { use =>
      val connector = use(new GrpcConnector)

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

      val mutableBlockchain = new MutableBlockchain(nodeSettings.blockchainSettings, blockchainApi)
      val start             = mutableBlockchain.height - 2
      val end               = mutableBlockchain.height + 2

      log.info("input: {}", input)
      val events = blockchainApi.stream
        .takeWhile {
          case Event.Closed      => false
          case Event.Next(event) => event.getUpdate.height < end
          case _ => false
        }
        .foreach {
          case Event.Failed(error) => println(error)
          case Event.Closed        => println("Closed")
          case Event.Next(event) =>
            val update = event.getUpdate
            update.update match {
              case Update.Empty =>

              case Update.Append(append) =>
                mutableBlockchain.setHeight(update.height)

                val stateUpdate = (append.getStateUpdate +: append.transactionStateUpdates).view

                stateUpdate.flatMap(_.assets).map(_.getAfter).foreach(mutableBlockchain.replaceAssetDescription)
                stateUpdate.flatMap(_.balances).foreach(mutableBlockchain.replaceBalance)
                stateUpdate.flatMap(_.leasingForAddress).foreach(mutableBlockchain.replaceLeasing)
                stateUpdate.flatMap(_.dataEntries).foreach(mutableBlockchain.replaceAccountData)

                val txs = append.body match {
                  case Body.Block(block)           => block.getBlock.transactions
                  case Body.MicroBlock(microBlock) => microBlock.getMicroBlock.getMicroBlock.transactions
                  case Body.Empty                  => Seq.empty
                }

                val dataUpdates = txs.view.map(_.transaction).flatMap {
                  case Transaction.WavesTransaction(tx) =>
                    tx.data match {
                      case Data.DataTransaction(txData) => txData.data
                      case _ => Nil
                    }
                  case _ => Nil
                }

//                log.info(
//                  s"${update.height}: assets=${stateUpdate.assets.size}, balances=${stateUpdate.balances.size}, " +
//                    s"leasingForAddress=${stateUpdate.leasingForAddress.size}, dataEntries=${stateUpdate.dataEntries.size}, " +
//                    s"dataUpdates=${dataUpdates.size}: ${dataUpdates.map(x => s"${x.key} -> ${x.value}").mkString(", ")}"
//                )

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
                  .foreach(Function.tupled(mutableBlockchain.replaceAccountScript))

                append.transactionIds.foreach(mutableBlockchain.replaceTransactionMeta(_, update.height))

                // Run
                val apiResult = execute(
                  mutableBlockchain,
                  input.request
                )

                // %d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d%d__3092089__34390720__0__1051594096626912__32516225386450__
                // 118605367995696__541091757276709__659697125272405__274154__2739486910802__2835679375701__
                // 29543498__9554543__1000000000__1000000__1000000__89526355538909
                log.info(s"apiResult: ${apiResult.value("result").as[JsObject].value("value")}")

              case Update.Rollback(rollback) =>
                log.info("Rollback, ignore")
            }
        }(Scheduler(commonScheduler))

      blockchainApi.watchBlockchainUpdates(blockchainUpdatesApiChannel, start)

      Await.result(events, Duration.Inf)
    }

    r match {
      case Failure(e) => log.error("Got an error", e)
      case _          => log.info("Done")
    }
  }
}
