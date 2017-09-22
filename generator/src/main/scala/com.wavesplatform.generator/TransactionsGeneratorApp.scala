package com.wavesplatform.generator

import java.net.InetSocketAddress
import java.util.concurrent.{Executors, ThreadLocalRandom}

import cats.implicits.showInterpolator
import com.typesafe.config.ConfigFactory
import com.wavesplatform.generator.cli.ScoptImplicits
import com.wavesplatform.generator.config.FicusImplicits
import com.wavesplatform.network.RawBytes
import com.wavesplatform.network.client.NetworkSender
import io.netty.channel.Channel
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.{EnumerationReader, NameMapper}
import org.slf4j.LoggerFactory
import scopt.OptionParser
import scorex.account.AddressScheme
import scorex.utils.LoggerFacade

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object TransactionsGeneratorApp extends App with ScoptImplicits with FicusImplicits with EnumerationReader {

  implicit val readConfigInHyphen: NameMapper = net.ceedubs.ficus.readers.namemappers.implicits.hyphenCase // IDEA bug

  val log = LoggerFacade(LoggerFactory.getLogger("generator"))

  val parser = new OptionParser[GeneratorSettings]("generator") {
    head("TransactionsGenerator - Waves load testing transactions generator")
    opt[Int]('i', "iterations").valueName("<iterations>").text("number of iterations").action { (v, c) =>
      c.copy(iterations = v)
    }
    opt[FiniteDuration]('d', "delay").valueName("<delay>").text("delay between iterations").action { (v, c) =>
      c.copy(delay = v)
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

      val workers = finalConfig.sendTo.map { node =>
        generateAndSend(node, finalConfig.addressScheme, finalConfig.iterations, finalConfig.delay, generator)
      }

      Future.sequence(workers).onComplete { _ =>
        log.info("Done all")
        threadPool.shutdown()
      }
  }

  private def generateAndSend(node: InetSocketAddress,
                              chainId: Char,
                              iterations: Int,
                              delay: FiniteDuration,
                              generator: TransactionGenerator)
                             (implicit ec: ExecutionContext): Future[Unit] = {
    val nonce = ThreadLocalRandom.current.nextLong()
    val sender = new NetworkSender(chainId, "generator", nonce)
    sys.addShutdownHook(sender.close())

    def sendTransactions(channel: Channel): Future[Unit] = {
      def loop(step: Int): Future[Unit] = {
        log.info(s"[$node] Iteration $step")
        val messages: Seq[RawBytes] = generator.next.map(tx => RawBytes(25.toByte, tx.bytes)).toSeq

        sender
          .send(channel, messages: _*)
          .andThen {
            case Success(_) => log.info(s"[$node] Transactions had been sent")
            case Failure(e) => log.error(s"[$node] An error during sending transations", e)
          }
          .map { _ =>
            blocking {
              if (step != iterations) {
                log.info(s"[$node] Sleeping for $delay")
                Thread.sleep(delay.toMillis)
              }
            }
          }
          .flatMap { _ =>
            if (step < iterations) loop(step + 1) else {
              log.info(s"[$node] Done")
              Future.successful(())
            }
          }
      }

      loop(1)
    }

    sender
      .connect(node)
      .flatMap(sendTransactions)
      .recover {
        case e => log.error(s"[$node] Failed to send transactions", e)
      }
      .andThen {
        case _ => sender.close()
      }
  }

}
