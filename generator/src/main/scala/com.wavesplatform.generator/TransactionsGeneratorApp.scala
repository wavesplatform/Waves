package com.wavesplatform.generator

import java.io.IOException
import java.net.InetSocketAddress
import java.util.concurrent.Executors

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
import scala.util.control.NonFatal
import scala.util.{Failure, Random, Success}

object TransactionsGeneratorApp extends App with ScoptImplicits with FicusImplicits with EnumerationReader {

  private val DelayBetweenReconnects = 3.seconds

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
    opt[Boolean]('r', "auto-reconnect").valueName("<true|false>").text("reconnect on errors").action { (v, c) =>
      c.copy(autoReconnect = v)
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

      val sender = new NetworkSender(finalConfig.addressScheme, "generator", nonce = Random.nextLong())
      sys.addShutdownHook(sender.close())

      val workers = finalConfig.sendTo.map { node =>
        generateAndSend(sender, node, finalConfig, generator)
      }

      def close(status: Int): Unit = {
        sender.close()
        threadPool.shutdown()
        System.exit(status)
      }

      Future
        .sequence(workers)
        .onComplete {
          case Success(_) =>
            log.info("Done")
            close(0)

          case Failure(e) =>
            log.error("Failed", e)
            close(1)
        }
  }

  private def generateAndSend(sender: NetworkSender,
                              node: InetSocketAddress,
                              settings: GeneratorSettings,
                              generator: TransactionGenerator)
                             (implicit ec: ExecutionContext): Future[Unit] = {
    import cats.data._
    import cats.implicits._

    type Result[T] = EitherT[Future, (Int, Throwable), T]

    def sendTransactions(startStep: Int, channel: Channel): Result[Unit] = {
      def loop(step: Int): Result[Unit] = {
        log.info(s"[$node] Iteration $step")
        val messages: Seq[RawBytes] = generator.next.map(tx => RawBytes(25.toByte, tx.bytes)).toSeq

        def trySend: Result[Unit] = EitherT {
          sender
            .send(channel, messages: _*)
            .map { _ => Right(()) }
            .recover {
              case NonFatal(e) => Left(step -> e)
            }
        }

        def next: Result[Unit] = {
          log.info(s"[$node] Transactions had been sent")
          if (step < settings.iterations) {
            log.info(s"[$node] Sleeping for ${settings.delay}")
            blocking {
              Thread.sleep(settings.delay.toMillis)
            }
            loop(step + 1)
          } else {
            log.info(s"[$node] Done")
            EitherT.right(Future.successful(Right(())))
          }
        }

        trySend.flatMap(_ => next)
      }

      loop(startStep)
    }

    def tryConnect: Result[Channel] = EitherT {
      sender
        .connect(node)
        .map { x => Right(x) }
        .recover {
          case NonFatal(e) => Left(0 -> e)
        }
    }

    def guardedSend(startStep: Int): Result[Unit] = {
      tryConnect
        .flatMap(sendTransactions(startStep, _))
        .recoverWith {
          case (_, e) if !settings.autoReconnect =>
            log.error("Stopping because autoReconnect is disabled", e)
            EitherT.left(Future.failed(new IOException(s"Errors during sending transactions to $node", e)))

          case (lastStep, NonFatal(e)) if lastStep < settings.iterations =>
            log.error(s"[$node] An error during sending transations, reconnect", e)
            blocking {
              Thread.sleep(DelayBetweenReconnects.toMillis)
            }
            guardedSend(lastStep)
        }
    }

    guardedSend(1)
      .leftMap { _ => () }
      .merge
  }

}
