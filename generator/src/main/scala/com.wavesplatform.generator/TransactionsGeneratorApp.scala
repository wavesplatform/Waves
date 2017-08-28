package com.wavesplatform.generator

import java.io.File
import java.net.{InetSocketAddress, URI}

import com.wavesplatform.generator.GeneratorSettings._
import com.wavesplatform.it.util.NetworkSender
import com.wavesplatform.network.RawBytes
import org.slf4j.LoggerFactory
import scopt.OptionParser
import scorex.account.AddressScheme
import scorex.utils.LoggerFacade

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

object Mode extends Enumeration {
  type Mode = Value
  val WIDE, NARROW = Value
}

case class GenerationParameters(mode: Mode.Value = Mode.NARROW,
                                config: Option[File] = None,
                                transactions: Int,
                                iterations: Int,
                                delay: Duration,
                                node: Option[InetSocketAddress] = None)

object TransactionsGeneratorApp extends App {

  import scala.concurrent.ExecutionContext.Implicits.global

  val log = LoggerFacade(LoggerFactory.getLogger("generator"))

  implicit val modeRead: scopt.Read[Mode.Value] = scopt.Read.reads(Mode withName _.toUpperCase)

  implicit val inetSocketAddressRead: scopt.Read[InetSocketAddress] = scopt.Read.reads(s => {
    val uri = new URI(s"my://$s")
    new InetSocketAddress(uri.getHost, uri.getPort)
  })

  val defaultConfig = fromConfig(readConfig(None))

  AddressScheme.current = new AddressScheme {
    override val chainId: Byte = defaultConfig.chainId.toByte
  }

  val parser = new OptionParser[GenerationParameters]("generator") {
    head("TransactionsGenerator - Waves load testing transactions generator")
    opt[Mode.Value]('m', "mode") valueName "<mode>" action { (v, c) => c.copy(mode = v) } text "generation mode (NARROW|WIDE)"
    opt[File]('c', "config") valueName "<config>" action { (v, c) => c.copy(config = Some(v)) } text "configuration file name"
    opt[Int]('t', "transactions") valueName "<transactions>" action { (v, c) => c.copy(transactions = v) } text "number of transactions to generate per iteration"
    opt[Int]('i', "iterations") valueName "<iterations>" action { (v, c) => c.copy(iterations = v) } text "number of iterations"
    opt[Duration]('d', "delay") valueName "<delay>" action { (v, c) => c.copy(delay = v) } text "delay between iterations"
    opt[InetSocketAddress]('n', "node") valueName "<node>" action { (v, c) => c.copy(node = Some(v)) } text "node address (IP:PORT) to send transactions to"
    help("help") text "display this help message"
  }

  val initialParameters = GenerationParameters(
    transactions = defaultConfig.transactions,
    iterations = defaultConfig.iterations,
    delay = defaultConfig.delay
  )

  parser.parse(args, initialParameters) match {
    case Some(parameters) =>
      val generator = parameters.mode match {
        case Mode.NARROW => new NarrowTransactionGenerator(defaultConfig.txProbabilities, defaultConfig.accounts)
        case Mode.WIDE => new WideTransactionGenerator(defaultConfig.accounts)
      }

      val node = parameters.node.getOrElse(defaultConfig.sendTo)
      generateAndSend(generator, parameters.transactions, parameters.iterations, parameters.delay, node, defaultConfig.chainId)
    case None => parser.failure("Failed to parse command line parameters")
  }

  private def generateAndSend(generator: TransactionGenerator, count: Int, iterations: Int, delay: Duration, node: InetSocketAddress, chainId: Char) = {
    log.info(s"Going to perform $iterations iterations")
    log.info(s"Generating $count transactions per iteration")
    log.info(s"With $delay between iterations")
    log.info(s"Source addresses: ${generator.accounts.mkString(", ")}")

    val nonce = Random.nextLong()
    val sender = new NetworkSender(chainId, "generator", nonce)
    sys.addShutdownHook(sender.close())

    val f = sender.connect(node).transform {
      case Success(channel) =>
        (1 to iterations).foreach { i =>
          log.info(s"Iteration $i")
          val transactions = generator.generate(count)
          Await.result(
            sender.send(channel, transactions.map(tx => RawBytes(25.toByte, tx.bytes)): _*)
              .map(_ => log.info("Transactions had been sent")), delay
          )
          log.info(s"Sleeping for $delay")
          if (i != iterations) Thread.sleep(delay.toMillis)
        }
        log.info("Done")
        Success(())
      case Failure(e) =>
        log.error(s"Failed to establish connection to $node", e)
        Success(())
    }
    Await.result(f, Duration.Inf)
    sender.close()
  }

}