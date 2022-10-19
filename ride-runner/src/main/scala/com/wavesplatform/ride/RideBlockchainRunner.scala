package com.wavesplatform.ride

import com.google.common.util.concurrent.ThreadFactoryBuilder
import com.wavesplatform.Application
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.common.utils.{Base64, EitherExt2}
import com.wavesplatform.grpc.{BlockchainGrpcApi, GrpcClientSettings, GrpcConnector}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.resources.*
import com.wavesplatform.utils.ScorexLogging

import java.io.File
import java.util.concurrent.Executors
import scala.concurrent.duration.DurationInt
import scala.io.Source
import scala.util.{Failure, Using}

object RideBlockchainRunner extends ScorexLogging {
  def main(args: Array[String]): Unit = {
    val basePath     = args(0)
    val nodeSettings = Application.loadApplicationConfig(Some(new File(s"$basePath/node/waves.conf")))

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = 'W'.toByte
    }

    val input = RideRunnerInput.parse(Using(Source.fromFile(new File(s"$basePath/input.json")))(_.getLines().mkString("\n")).get)
    input.request.call.foreach { call =>
      val script = Script.fromBase64String(Base64.encode(compiledScript.bytes)).explicitGet()
      val same   = input.accounts(call.dApp.asInstanceOf[Address]).scriptInfo.exists(_.script == script)
      require(same, "Scripts are not same!")
    }

    val r = Using.Manager { use =>
      val connector = use(new GrpcConnector)
      val channel = connector.mkChannel(
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

      val commonScheduler = use(
        Executors.newScheduledThreadPool(
          2,
          new ThreadFactoryBuilder().setNameFormat("common-scheduler-%d").setDaemon(false).build()
        )
      )

      val blockchainApi = use(
        new BlockchainGrpcApi(
          settings = BlockchainGrpcApi.Settings(1.minute),
          grpcApiChannel = channel,
          hangScheduler = commonScheduler
        )
      )

      val mutableBlockchain = new MutableBlockchain(nodeSettings.blockchainSettings, blockchainApi)

      val apiResult = execute(
        mutableBlockchain,
        input.request
      )

      log.info(s"apiResult: $apiResult")
    }

    r match {
      case Failure(e) => log.error("Got an error", e)
      case _          => log.info("Done")
    }
  }
}
