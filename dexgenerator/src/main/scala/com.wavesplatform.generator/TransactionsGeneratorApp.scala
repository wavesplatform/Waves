package com.wavesplatform.generator

import java.util.concurrent.Executors

import cats.implicits.showInterpolator
import com.typesafe.config.ConfigFactory
import com.wavesplatform.generator.cli.ScoptImplicits
import com.wavesplatform.generator.config.FicusImplicits
import com.wavesplatform.generator.utils.{ApiRequests, GenOrderType}
import com.wavesplatform.generator.workers.{FakeOrdersWorker, OrdersWorker}
import com.wavesplatform.it.util.GlobalTimer
import com.wavesplatform.network.client.NetworkSender
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.{EnumerationReader, NameMapper}
import org.asynchttpclient.AsyncHttpClient
import org.asynchttpclient.Dsl.{config => clientConfig, _}
import org.asynchttpclient._
import org.slf4j.LoggerFactory
import scopt.OptionParser
import scorex.account.AddressScheme
import scorex.utils.LoggerFacade
import settings.GeneratorSettings

import scala.collection.immutable
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

object TransactionsGeneratorApp extends App with ScoptImplicits with FicusImplicits with EnumerationReader {

  implicit val readConfigInHyphen: NameMapper = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase // IDEA bug

  val log = LoggerFacade(LoggerFactory.getLogger("generator"))

  val parser = new OptionParser[GeneratorSettings]("generator") {
    head("TransactionsGenerator - Waves load testing transactions generator")
    opt[Int]('i', "iterations").valueName("<iterations>").text("number of iterations").action { (v, c) =>
      c.copy(worker = c.worker.copy(iterations = v))
    }
    opt[FiniteDuration]('d', "delay").valueName("<delay>").text("delay between iterations").action { (v, c) =>
      c.copy(worker = c.worker.copy(delay = v))
    }
    opt[Boolean]('r', "auto-reconnect").valueName("<true|false>").text("reconnect on errors").action { (v, c) =>
      c.copy(worker = c.worker.copy(autoReconnect = v))
    }
    help("help").text("display this help message")

    cmd("dex")
      .action { (_, c) => c.copy(mode = Mode.DEX) }
      .text("Run orders between pre-defined accounts")
      .children(
        opt[Int]("orders").abbr("t").optional().text("number of orders").action { (x, c) =>
          c.copy(dex = c.dex.copy(orders = x))
        }
      )
  }

  val defaultConfig = ConfigFactory.load().as[GeneratorSettings]("generator")
  parser.parse(args, defaultConfig) match {
    case None => parser.failure("Failed to parse command line parameters")
    case Some(finalConfig) =>
      log.info(show"The final configuration: \n$finalConfig")

      AddressScheme.current = new AddressScheme {
        override val chainId: Byte = finalConfig.addressScheme.toByte
      }

      val generator: OrdersGenerator = finalConfig.mode match {
        case Mode.DEX => new OrdersGenerator(finalConfig.dex, finalConfig.richPrivateKeyAccounts)
      }

      val ordersDistr: Map[GenOrderType.Value, Double] = finalConfig.dex.probabilities.mapValues(v => v * finalConfig.dex.orders)

      val threadPool = Executors.newFixedThreadPool(Math.max(1, finalConfig.sendTo.size))
      implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(threadPool)

      val sender = new NetworkSender(finalConfig.addressScheme, "generator", nonce = Random.nextLong())
      sys.addShutdownHook(sender.close())


      val client: AsyncHttpClient = asyncHttpClient(clientConfig().setKeepAlive(false).setNettyTimer(GlobalTimer.instance))

      val t = new ApiRequests(client)


      val test: GeneratorSettings = finalConfig


      val defaultPrice = 1000



      val workers: Seq[Worker] = ordersDistr.foreach(p =>
        new Worker(finalConfig.worker, finalConfig, finalConfig.matcherConfig.endpoint, tradingAssets, p._1, p._2, client))


      def close(status: Int): Unit = {
        sender.close()
        threadPool.shutdown()
        System.exit(status)
      }

      Future
        .sequence(workers.map(_.run()))
        .onComplete {
          case Success(_) =>
            log.info("Done")
            close(0)

          case Failure(e) =>
            log.error("Failed", e)
            close(1)
        }
  }

}
