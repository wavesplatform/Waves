package com.wavesplatform.generator

import java.io.File
import java.net.InetSocketAddress
import java.util.concurrent.Executors

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

import cats.implicits.showInterpolator
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.generator.GeneratorSettings.NodeAddress
import com.wavesplatform.generator.Preconditions.{PGenSettings, UniverseHolder}
import com.wavesplatform.generator.cli.ScoptImplicits
import com.wavesplatform.generator.config.FicusImplicits
import com.wavesplatform.generator.utils.Universe
import com.wavesplatform.network.client.NetworkSender
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.utils.{LoggerFacade, NTP}
import com.wavesplatform.Application
import monix.execution.Scheduler
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{EnumerationReader, NameMapper, ValueReader}
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import org.asynchttpclient.AsyncHttpClient
import org.asynchttpclient.Dsl.asyncHttpClient
import org.slf4j.LoggerFactory
import scopt.OptionParser

object TransactionsGeneratorApp extends App with ScoptImplicits with FicusImplicits with EnumerationReader {

  // IDEA bugs
  implicit val inetSocketAddressReader: ValueReader[InetSocketAddress] = com.wavesplatform.settings.inetSocketAddressReader
  implicit val readConfigInHyphen: NameMapper                          = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase
  implicit val httpClient: AsyncHttpClient                             = asyncHttpClient()

  val log = LoggerFacade(LoggerFactory.getLogger("generator"))

  val parser = new OptionParser[GeneratorSettings]("generator") {
    head("TransactionsGenerator - Waves load testing transactions generator")
    opt[File]('c', "configuration").valueName("<file>").text("generator configuration path")
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
        }
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
        }
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

  val configParamParser = new OptionParser[File]("configuration") {
    opt[String]('c', "configuration").action { case (c, _) => new File(c) }
    override def errorOnUnknownArgument: Boolean  = false
    override def reportWarning(msg: String): Unit = ()
  }

  val externalConf =
    configParamParser
      .parse(args, new File("generator.local.conf"))
      .getOrElse(throw new RuntimeException("Failed to parse configuration path from command line parameters"))

  val wavesSettings = Application.loadApplicationConfig(if (externalConf.isFile) Some(externalConf) else None)

  val defaultConfig =
    wavesSettings.config
      .as[GeneratorSettings]("waves.generator")

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

      val estimator = wavesSettings.estimator

      val (universe, initialUniTransactions, initialTailTransactions) = preconditions
        .fold((UniverseHolder(), List.empty[Transaction], List.empty[Transaction]))(Preconditions.mk(_, time, estimator))

      Universe.Accounts = universe.accounts
      Universe.IssuedAssets = universe.issuedAssets
      Universe.Leases = universe.leases

      val generator: TransactionGenerator = finalConfig.mode match {
        case Mode.NARROW   => NarrowTransactionGenerator(finalConfig.narrow, finalConfig.privateKeyAccounts, time, estimator)
        case Mode.WIDE     => new WideTransactionGenerator(finalConfig.wide, finalConfig.privateKeyAccounts)
        case Mode.DYN_WIDE => new DynamicWideTransactionGenerator(finalConfig.dynWide, finalConfig.privateKeyAccounts)
        case Mode.MULTISIG => new MultisigTransactionGenerator(finalConfig.multisig, finalConfig.privateKeyAccounts, estimator)
        case Mode.ORACLE   => new OracleTransactionGenerator(finalConfig.oracle, finalConfig.privateKeyAccounts, estimator)
        case Mode.SWARM    => new SmartGenerator(finalConfig.swarm, finalConfig.privateKeyAccounts, estimator)
        case _             => ???
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

      val initialGenTransactions     = generator.initial
      val initialGenTailTransactions = generator.tailInitial

      log.info(s"Universe precondition transactions size: ${initialUniTransactions.size}")
      log.info(s"Generator precondition transactions size: ${initialGenTransactions.size}")
      log.info(s"Universe precondition tail transactions size: ${initialTailTransactions.size}")
      log.info(s"Generator precondition tail transactions size: ${initialGenTailTransactions.size}")

      val workers = finalConfig.sendTo.map { case NodeAddress(node, nodeRestUrl) =>
        log.info(s"Creating worker: ${node.getHostString}:${node.getPort}")
        // new Worker(finalConfig.worker, sender, node, generator, initialTransactions.map(RawBytes.from))
        new Worker(
          finalConfig.worker,
          Iterator.continually(generator.next()).flatten,
          sender,
          node,
          nodeRestUrl,
          () => canContinue,
          initialUniTransactions ++ initialGenTransactions,
          finalConfig.privateKeyAccounts.map(_.toAddress.toString),
          initialTailTransactions ++ initialGenTailTransactions
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
