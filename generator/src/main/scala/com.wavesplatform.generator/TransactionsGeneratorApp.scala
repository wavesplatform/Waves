package com.wavesplatform.generator

import java.net.InetSocketAddress
import java.util.concurrent.{Executors, ThreadLocalRandom}

import com.wavesplatform.generator.GeneratorSettings._
import com.wavesplatform.it.util.NetworkSender
import com.wavesplatform.network.RawBytes
import io.netty.channel.Channel
import org.slf4j.LoggerFactory
import scopt.OptionParser
import scorex.account.AddressScheme
import scorex.utils.LoggerFacade

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.{Failure, Success}

object Mode extends Enumeration {
  type Mode = Value
  val WIDE, NARROW = Value
}

case class GenerationParameters(mode: Mode.Value,
                                transactions: Int,
                                iterations: Int,
                                delay: FiniteDuration)

object TransactionsGeneratorApp extends App {

  val log = LoggerFacade(LoggerFactory.getLogger("generator"))

  implicit val modeRead: scopt.Read[Mode.Value] = scopt.Read.reads(Mode withName _.toUpperCase)

  private implicit val finiteDurationRead: scopt.Read[FiniteDuration] = scopt.Read.durationRead.map { x =>
    if (x.isFinite()) FiniteDuration(x.length, x.unit)
    else throw new IllegalArgumentException(s"Duration '$x' expected to be finite")
  }


  val parser = new OptionParser[GenerationParameters]("generator") {
    head("TransactionsGenerator - Waves load testing transactions generator")
    opt[Mode.Value]('m', "mode") valueName "<mode>" action { (v, c) => c.copy(mode = v) } text "generation mode (NARROW|WIDE)"
    opt[Int]('t', "transactions") valueName "<transactions>" action { (v, c) => c.copy(transactions = v) } text "number of transactions to generate per iteration"
    opt[Int]('i', "iterations") valueName "<iterations>" action { (v, c) => c.copy(iterations = v) } text "number of iterations"
    opt[FiniteDuration]('d', "delay") valueName "<delay>" action { (v, c) => c.copy(delay = v) } text "delay between iterations"
    help("help") text "display this help message"
  }

  val defaultConfig = fromConfig(readConfig(None))

  val initialParameters = GenerationParameters(
    mode = Mode.NARROW,
    transactions = 1000,
    iterations = 1,
    delay = 1.minutes
  )

  parser.parse(args, initialParameters) match {
    case Some(parameters) =>
      val actualConfig = fromConfig(readConfig(None))

      AddressScheme.current = new AddressScheme {
        override val chainId: Byte = actualConfig.chainId.toByte
      }
      val generator = parameters.mode match {
        case Mode.NARROW => new NarrowTransactionGenerator(actualConfig.txProbabilities, actualConfig.accounts)
        case Mode.WIDE => new WideTransactionGenerator(actualConfig.accounts)
      }

      val nodes = actualConfig.sendTo
      val threadPool = Executors.newFixedThreadPool(Math.max(1, nodes.size))
      implicit val ec = ExecutionContext.fromExecutor(threadPool)

      val workers = nodes.map { node =>
        generateAndSend(generator, parameters.transactions, parameters.iterations, parameters.delay, node, actualConfig.chainId)
      }

      Future.sequence(workers).onComplete { _ =>
        log.info("Done all")
        threadPool.shutdown()
      }
    case None => parser.failure("Failed to parse command line parameters")
  }

  private def generateAndSend(generator: TransactionGenerator, count: Int, iterations: Int, delay: FiniteDuration,
                              node: InetSocketAddress, chainId: Char)
                             (implicit ec: ExecutionContext): Future[Unit] = {
    log.info(s"[$node] Going to perform $iterations iterations")
    log.info(s"[$node] Generating $count transactions per iteration")
    log.info(s"[$node] With $delay between iterations")
    log.info(s"[$node] Source addresses: ${generator.accounts.mkString(", ")}")

    val nonce = ThreadLocalRandom.current.nextLong()
    val sender = new NetworkSender(chainId, "generator", nonce)
    sys.addShutdownHook(sender.close())

    def sendTransactions(channel: Channel): Future[Unit] = {
      def loop(step: Int): Future[Unit] = {
        log.info(s"[$node] Iteration $step")
        val transactions = generator.generate(count)
        val messages = transactions.map(tx => RawBytes(25.toByte, tx.bytes))

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