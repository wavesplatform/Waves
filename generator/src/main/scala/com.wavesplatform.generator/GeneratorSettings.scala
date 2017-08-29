package com.wavesplatform.generator

import java.io.File
import java.net.InetSocketAddress

import com.google.common.base.CaseFormat
import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.settings.loadConfig
import net.ceedubs.ficus.Ficus._
import org.slf4j.LoggerFactory
import scorex.account.PrivateKeyAccount
import scorex.crypto.encode.Base58
import scorex.transaction.TransactionParser
import scorex.transaction.TransactionParser.TransactionType
import scorex.utils.LoggerFacade

import scala.concurrent.duration.FiniteDuration

case class GeneratorSettings(chainId: Char,
                             accounts: Seq[PrivateKeyAccount],
                             transactions: Int,
                             iterations: Int,
                             delay: FiniteDuration,
                             txProbabilities: Map[TransactionParser.TransactionType.Value, Float],
                             sendTo: InetSocketAddress)

object GeneratorSettings {
  val configPath: String = "generator"

  def fromConfig(config: Config): GeneratorSettings = {
    val converter = CaseFormat.LOWER_HYPHEN.converterTo(CaseFormat.UPPER_CAMEL)

    def toTxType(key: String): TransactionType.Value =
      TransactionType.withName(s"${converter.convert(key)}Transaction")

    val chainId = config.as[String](s"$configPath.chainId").head
    val accounts = config.as[List[String]](s"$configPath.accounts").map(s => PrivateKeyAccount(Base58.decode(s).get))
    val transactions = config.as[Int](s"$configPath.transactions")
    val iterations = config.as[Int](s"$configPath.iterations")
    val delay = config.as[FiniteDuration](s"$configPath.delay")
    val txProbabilities = config.as[Map[String, Double]](s"$configPath.probabilities").map(kv => toTxType(kv._1) -> kv._2.toFloat)
    val sendTo = new InetSocketAddress(config.as[String](s"$configPath.send-to.address"), config.as[Int](s"$configPath.send-to.port"))

    GeneratorSettings(chainId, accounts, transactions, iterations, delay, txProbabilities, sendTo)
  }

  private val log = LoggerFacade(LoggerFactory.getLogger(getClass))

  def readConfig(userConfigPath: Option[String]): Config = {
    val maybeConfigFile = for {
      maybeFilename <- userConfigPath
      file = new File(maybeFilename)
      if file.exists
    } yield file

    val config = maybeConfigFile match {
      // if no user config is supplied, the library will handle overrides/application/reference automatically
      case None =>
        ConfigFactory.load()
      // application config needs to be resolved wrt both system properties *and* user-supplied config.
      case Some(file) =>
        val cfg = ConfigFactory.parseFile(file)
        if (!cfg.hasPath("generator")) {
          log.error("Malformed configuration file was provided! Aborting!")
          System.exit(1)
        }
        loadConfig(cfg)
    }
    config
  }
}
