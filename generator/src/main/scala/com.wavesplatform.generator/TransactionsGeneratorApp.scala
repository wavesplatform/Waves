package com.wavesplatform.generator

import java.util.concurrent.Executors

import cats.implicits.showInterpolator
import com.typesafe.config.ConfigFactory
import com.wavesplatform.generator.cli.ScoptImplicits
import com.wavesplatform.generator.config.FicusImplicits
import com.wavesplatform.network.client.NetworkSender
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.{EnumerationReader, NameMapper}
import org.slf4j.LoggerFactory
import scopt.OptionParser
import scorex.account.AddressScheme
import scorex.utils.LoggerFacade

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

    cmd("narrow")
      .action { (_, c) => c.copy(mode = Mode.NARROW) }
      .text("Run transactions between pre-defined accounts")
      .children(
        opt[Int]("transactions").abbr("t").optional().text("number of transactions").action { (x, c) =>
          c.copy(narrow = c.narrow.copy(transactions = x))
        }
      )

    cmd("wide")
      .action { (_, c) => c.copy(mode = Mode.WIDE) }
      .text("Run transactions those transfer funds to another accounts")
      .children(
        opt[Int]("transactions").abbr("t").optional().text("number of transactions").action { (x, c) =>
          c.copy(wide = c.wide.copy(transactions = x))
        },
        opt[Option[Int]]("limit-accounts").abbr("la").optional().text("limit recipients").action { (x, c) =>
          c.copy(wide = c.wide.copy(limitAccounts = x))
        }
      )

    cmd("dyn-wide")
      .action { (_, c) => c.copy(mode = Mode.DYN_WIDE) }
      .text("Like wide, but the number of transactions is changed during the iteration")
      .children(
        opt[Int]("start").abbr("s").optional().text("initial amount of transactions").action { (x, c) =>
          c.copy(dynWide = c.dynWide.copy(start = x))
        },
        opt[Double]("grow-adder").abbr("g").optional().action { (x, c) =>
          c.copy(dynWide = c.dynWide.copy(growAdder = x))
        },
        opt[Int]("max").abbr("m").optional().action { (x, c) =>
          c.copy(dynWide = c.dynWide.copy(maxTxsPerRequest = Some(x)))
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

      val generator: TransactionGenerator = finalConfig.mode match {
        case Mode.NARROW => new NarrowTransactionGenerator(finalConfig.narrow, finalConfig.privateKeyAccounts)
        case Mode.WIDE => new WideTransactionGenerator(finalConfig.wide, finalConfig.privateKeyAccounts)
        case Mode.DYN_WIDE => new DynamicWideTransactionGenerator(finalConfig.dynWide, finalConfig.privateKeyAccounts)
      }

      val threadPool = Executors.newFixedThreadPool(Math.max(1, finalConfig.sendTo.size))
      implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(threadPool)

      val sender = new NetworkSender(finalConfig.addressScheme, "generator", nonce = Random.nextLong())
      sys.addShutdownHook(sender.close())

      val workers = finalConfig.sendTo.map { node =>
        new Worker(finalConfig.worker, sender, node, generator)
      }

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
