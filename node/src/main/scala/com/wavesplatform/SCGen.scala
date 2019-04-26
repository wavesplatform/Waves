package com.wavesplatform

import java.io.{File, FileOutputStream, PrintStream}

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.database.LevelDBWriter
import com.wavesplatform.db.openDB
import com.wavesplatform.state.Height
import com.wavesplatform.utils.{Merkle, ScorexLogging}
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer
import play.api.libs.json.Json
import scopt.OParser

object SCGen extends App with ScorexLogging {

  final case class GenConfig(
      length: Int = 1999,
      configFile: Option[File] = None,
      outputFile: Option[File] = None
  )

  val builder = OParser.builder[GenConfig]
  val parser = {
    import builder._

    OParser
      .sequence(
        programName("scgen"),
        opt[Int]('l', "length")
          .text("Number of blocks")
          .required()
          .action((v, conf) => conf.copy(length = v)),
        opt[File]('c', "config")
          .text("Path to node config file")
          .action((v, conf) => conf.copy(configFile = Some(v))),
        opt[File]('o', "output")
          .text("Path to output file")
          .action((v, conf) => conf.copy(outputFile = Some(v)))
      )
  }

  OParser.parse(parser, args, GenConfig()) match {
    case Some(config) => generate(config)
    case None         => sys.exit(1)
  }

  def generate(config: GenConfig): Unit = {
    import com.wavesplatform.settings._

    val nodeConfigFile = config.configFile.getOrElse(new File("waves-testnet.conf"))

    val wavesSettings = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.parseFile(nodeConfigFile)))

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = wavesSettings.blockchainSettings.addressSchemeCharacter.toByte
    }

    val portfolioChanges = Observer.empty(UncaughtExceptionReporter.LogExceptionsToStandardErr)

    val db     = openDB(wavesSettings.dbSettings.directory)
    val reader = new LevelDBWriter(db, portfolioChanges, wavesSettings.blockchainSettings.functionalitySettings, wavesSettings.dbSettings)

//    val out = config.outputFile match {
//      case None    => System.out
//      case Some(f) => new PrintStream(f)
//    }

    val out: PrintStream = config.outputFile match {
      case Some(file) => new PrintStream(file)
      case None       => Console.out
    }

    val height = reader.height

    out.write('[')

    def loop(n: Int): Unit = {
      if (n < height) {
        reader.blockAt(n) foreach { block =>
          val (balances, effectiveBalances) = {
            val minerBalanceInfo = reader.minerBalancesAtHeight(Height @@ n)
            minerBalanceInfo.foldLeft((List.empty[(Address, Long)], List.empty[(Address, Long)])) {
              case ((_balances, _effectiveBalances), (address, balanceInfo)) =>
                val nextBalances = (address, balanceInfo.currentBalance) :: _balances
                val nextEffectiveBalances =
                  if (balanceInfo.miningBalance > 1000) (address, balanceInfo.miningBalance) :: _effectiveBalances else _effectiveBalances

                (nextBalances, nextEffectiveBalances)
            }
          }

          val txHash =
            if (block.transactionData.isEmpty) {
              Merkle.EMPTY_ROOT_HASH
            } else {
              Merkle.mkTxTree(block.transactionData).rootHash
            }
          val balanceHash = Merkle.mkMinerBalanceTree(balances).rootHash
          val effBalanceHash = Merkle.mkMinerBalanceTree(effectiveBalances).rootHash

          val json = block
            .copy(
              transactionTreeHash = txHash,
              minerWavesBalancesTreeHash = balanceHash,
              minerEffectiveBalancesTreeHash = effBalanceHash
            )
            .headerJson()

          out.write(Json.prettyPrint(json).getBytes())
          out.write(',')
        }

        loop(n + 1)
      } else {
        out.write('\n')
        out.write(']')
        out.flush()
        out.close()
      }
    }

    loop(height - config.length)
  }

  def loadChain(reader: LevelDBWriter, from: Int, to: Int): List[(Int, Block)] = {
    def loop(n: Int, acc: List[(Int, Block)]): List[(Int, Block)] = {
      if (n < to) {
        reader.blockAt(n) match {
          case None => {
            log.info(s"Last block reached at height: $n")
            acc.reverse
          }
          case Some(b) => loop(n + 1, (n, b) :: acc)
        }
      } else acc.reverse
    }

    loop(from, Nil)
  }
}
