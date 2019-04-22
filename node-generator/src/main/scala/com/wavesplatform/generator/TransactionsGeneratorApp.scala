package com.wavesplatform.generator

import java.util.concurrent.Executors

import cats.implicits.showInterpolator
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.generator.GeneratorSettings.NodeAddress
import com.wavesplatform.generator.Preconditions.{PGenSettings, UniverseHolder}
import com.wavesplatform.generator.cli.ScoptImplicits
import com.wavesplatform.generator.config.FicusImplicits
import com.wavesplatform.generator.utils.Universe
import com.wavesplatform.network.client.NetworkSender
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.{LoggerFacade, NTP}
import monix.execution.Scheduler
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.{EnumerationReader, NameMapper}
import org.asynchttpclient.Dsl.asyncHttpClient
import org.slf4j.LoggerFactory
import scopt.OptionParser

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

object TransactionsGeneratorApp extends App with ScoptImplicits with FicusImplicits with EnumerationReader {

  // IDEA bugs
  implicit val inetSocketAddressReader        = com.wavesplatform.settings.inetSocketAddressReader
  implicit val readConfigInHyphen: NameMapper = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
  implicit val httpClient                     = asyncHttpClient()

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
      .action { (_, c) =>
        c.copy(mode = Mode.NARROW)
      }
      .text("Run transactions between pre-defined accounts")
      .children(
        opt[Int]("transactions").abbr("t").optional().text("number of transactions").action { (x, c) =>
          c.copy(narrow = c.narrow.copy(transactions = x))
        }
      )

    cmd("wide")
      .action { (_, c) =>
        c.copy(mode = Mode.WIDE)
      }
      .text("Run transactions those transfer funds to another accounts")
      .children(
        opt[Int]("transactions").abbr("t").optional().text("number of transactions").action { (x, c) =>
          c.copy(wide = c.wide.copy(transactions = x))
        },
        opt[Option[Int]]("limit-accounts").abbr("la").optional().text("limit recipients").action { (x, c) =>
          c.copy(wide = c.wide.copy(limitDestAccounts = x))
        }
      )

    cmd("dyn-wide")
      .action { (_, c) =>
        c.copy(mode = Mode.DYN_WIDE)
      }
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
        },
        opt[Option[Int]]("limit-accounts").abbr("la").optional().text("limit recipients").action { (x, c) =>
          c.copy(dynWide = c.dynWide.copy(limitDestAccounts = x))
        }
      )

    cmd("multisig")
      .action { (_, c) =>
        c.copy(mode = Mode.MULTISIG)
      }
      .text("Multisig cycle of funding, initializng and sending funds back")
      .children(
        opt[Int]("transactions").abbr("t").optional().text("number of transactions").action { (x, c) =>
          c.copy(multisig = c.multisig.copy(transactions = x))
        },
        opt[Boolean]("first-run").abbr("first").optional().text("generate set multisig script transaction").action { (x, c) =>
          c.copy(multisig = c.multisig.copy(firstRun = x))
        },
      )

    cmd("oracle")
      .action { (_, c) =>
        c.copy(mode = Mode.ORACLE)
      }
      .text("Oracle load test")
      .children(
        opt[Int]("transactions").abbr("t").optional().text("number of transactions").action { (x, c) =>
          c.copy(oracle = c.oracle.copy(transactions = x))
        },
        opt[Boolean]("enabled").abbr("e").optional().text("DataEnty value").action { (x, c) =>
          c.copy(multisig = c.multisig.copy(firstRun = x))
        },
      )

    cmd("swarm")
      .action { (_, c) =>
        c.copy(mode = Mode.SWARM)
      }
      .text("SetScript load test")
      .children(
        opt[Int]("scripts").abbr("st").optional().text("number of SetScripts transactions").action { (x, c) =>
          c.copy(swarm = c.swarm.copy(scripts = x))
        },
        opt[Int]("transfers").abbr("tt").optional().text("number of Transfer transactions").action { (x, c) =>
          c.copy(swarm = c.swarm.copy(transfers = x))
        },
        opt[Boolean]("complexity").abbr("ct").optional().text(" script complexity").action { (x, c) =>
          c.copy(swarm = c.swarm.copy(complexity = x))
        },
        opt[Int]("exchange").abbr("et").optional().text("number of exchange transactions").action { (x, c) =>
          c.copy(swarm = c.swarm.copy(exchange = x))
        }
      )
  }

  val defaultConfig =
    ConfigFactory
      .load()
      .as[GeneratorSettings]("generator")

  val wavesSettings = WavesSettings.fromRootConfig(ConfigFactory.load())

  parser.parse(args, defaultConfig) match {
    case None => parser.failure("Failed to parse command line parameters")
    case Some(finalConfig) =>
      log.info(show"The final configuration: \n$finalConfig")

      AddressScheme.current = new AddressScheme {
        override val chainId: Byte = finalConfig.addressScheme.toByte
      }

      val time = new NTP("pool.ntp.org")

      val preconditions =
        ConfigFactory
          .load("preconditions.conf")
          .as[Option[PGenSettings]]("preconditions")(optionValueReader(Preconditions.preconditionsReader))

      val (universe, initialTransactions) = preconditions
        .fold((UniverseHolder(), List.empty[Transaction]))(Preconditions.mk(_, time))

      Universe.Accounts = universe.accounts
      Universe.IssuedAssets = universe.issuedAssets
      Universe.Leases = universe.leases

      val generator: TransactionGenerator = finalConfig.mode match {
        case Mode.NARROW   => new NarrowTransactionGenerator(finalConfig.narrow, finalConfig.privateKeyAccounts)
        case Mode.WIDE     => new WideTransactionGenerator(finalConfig.wide, finalConfig.privateKeyAccounts)
        case Mode.DYN_WIDE => new DynamicWideTransactionGenerator(finalConfig.dynWide, finalConfig.privateKeyAccounts)
        case Mode.MULTISIG => new MultisigTransactionGenerator(finalConfig.multisig, finalConfig.privateKeyAccounts)
        case Mode.ORACLE   => new OracleTransactionGenerator(finalConfig.oracle, finalConfig.privateKeyAccounts)
        case Mode.SWARM    => new SmartGenerator(finalConfig.swarm, finalConfig.privateKeyAccounts)
      }

      val threadPool                            = Executors.newFixedThreadPool(Math.max(1, finalConfig.sendTo.size))
      implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(threadPool)

      val sender = new NetworkSender(wavesSettings.networkSettings.trafficLogger, finalConfig.addressScheme, "generator", nonce = Random.nextLong())

      sys.addShutdownHook(sender.close())

      @volatile
      var canContinue = true

      sys.addShutdownHook {
        log.error("Stopping generator")
        canContinue = false
      }

      if (finalConfig.worker.workingTime > Duration.Zero) {
        log.info(s"Generator will be stopped after ${finalConfig.worker.workingTime}")

        Scheduler.global.scheduleOnce(finalConfig.worker.workingTime) {
          log.warn(s"Stopping generator after: ${finalConfig.worker.workingTime}")
          canContinue = false
        }
      }

      log.info(s"Preconditions: $initialTransactions")

      val workers = finalConfig.sendTo.map {
        case NodeAddress(node, nodeRestUrl) =>
          log.info(s"Creating worker: ${node.getHostString}:${node.getPort}")
          // new Worker(finalConfig.worker, sender, node, generator, initialTransactions.map(RawBytes.from))
          new NewWorker(
            finalConfig.worker,
            Iterator.continually(generator.next()).flatten,
            sender,
            node,
            nodeRestUrl,
            () => canContinue,
            initialTransactions
          )
      }

      def close(status: Int): Unit = {
        sender.close()
        time.close()
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
