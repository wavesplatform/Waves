package com.wavesplatform.generator

import java.net.InetSocketAddress
import java.util.concurrent.{Executors, ThreadLocalRandom}

import com.wavesplatform.generator.GeneratorSettings._
import com.wavesplatform.it.util.NetworkSender
import com.wavesplatform.network.RawBytes
import io.netty.channel.Channel
import org.slf4j.LoggerFactory
import scopt.{OptionParser, Read}
import scorex.account.{AddressScheme, PrivateKeyAccount}
import scorex.transaction.Transaction
import scorex.utils.LoggerFacade

import scala.concurrent.duration._
import scala.concurrent._
import scala.util.{Failure, Success}

object Mode extends Enumeration {
  type Mode = Value
  val WIDE, NARROW, DYN_WIDE = Value
}

object TransactionsGeneratorApp extends App {

  val log = LoggerFacade(LoggerFactory.getLogger("generator"))

  implicit def scoptReads[T](implicit tReads: scopt.Read[T]): scopt.Read[Option[T]] = new Read[Option[T]] {
    override val arity: Int = 1
    override val reads: String => Option[T] = {
      case "null" => None
      case x => Option(tReads.reads(x))
    }
  }

  implicit val modeRead: scopt.Read[Mode.Value] = scopt.Read.reads(Mode withName _.toUpperCase)

  private implicit val finiteDurationRead: scopt.Read[FiniteDuration] = scopt.Read.durationRead.map { x =>
    if (x.isFinite()) FiniteDuration(x.length, x.unit)
    else throw new IllegalArgumentException(s"Duration '$x' expected to be finite")
  }

  val parser = new OptionParser[GeneratorSettings]("generator") {
    head("TransactionsGenerator - Waves load testing transactions generator")
    opt[Int]('i', "iterations").valueName("<iterations>").text("number of iterations").action { (v, c) => c.copy(iterations = v) }
    opt[FiniteDuration]('d', "delay").valueName("<delay>").text("delay between iterations").action { (v, c) => c.copy(delay = v) }
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
        opt[Double]("grow-factor").abbr("g").optional().action { (x, c) =>
          c.copy(dynWide = c.dynWide.copy(growFactor = x))
        }
      )
  }

  val defaultConfig = fromConfig(readConfig(None))
  parser.parse(args, defaultConfig) match {
    case Some(finalConfig) =>
      println(finalConfig)
      AddressScheme.current = new AddressScheme {
        override val chainId: Byte = finalConfig.chainId.toByte
      }

      val generator = finalConfig.mode match {
        case Mode.NARROW => new NarrowTransactionGenerator(finalConfig.narrow, finalConfig.accounts)
        case Mode.WIDE => new WideTransactionGenerator(finalConfig.wide, finalConfig.accounts)
        case Mode.DYN_WIDE => new DynamicWideTransactionGenerator(finalConfig.dynWide, finalConfig.accounts)
      }

//      val threadPool = Executors.newFixedThreadPool(Math.max(1, finalConfig.sendTo.size))
//      implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(threadPool)
//
//      val workers = finalConfig.sendTo.map { node =>
//        generateAndSend(generator, parameters.transactions, parameters.iterations, parameters.delay, node, actualConfig.chainId)
//      }
//
//      Future.sequence(workers).onComplete { _ =>
//        log.info("Done all")
//        threadPool.shutdown()
//      }
    case None => parser.failure("Failed to parse command line parameters")
  }

  private def generateAndSend(node: InetSocketAddress,
                              chainId: Char,
                              iterations: Int,
                              delay: FiniteDuration,
                              generator: Iterator[Iterator[Transaction]])
                             (implicit ec: ExecutionContext): Future[Unit] = {
    log.info(s"[$node] Going to perform $iterations iterations")
    //log.info(s"[$node] Generating $count transactions per iteration")
    log.info(s"[$node] With $delay between iterations")
    //log.info(s"[$node] Source addresses: ${generator.accounts.mkString(", ")}")

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