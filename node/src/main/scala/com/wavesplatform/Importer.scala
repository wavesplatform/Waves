package com.wavesplatform

import java.io._

import com.google.common.primitives.Ints
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.db.openDB
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.settings.{WavesSettings, loadConfig}
import com.wavesplatform.state.Portfolio
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils._
import com.wavesplatform.utx.UtxPool
import monix.execution.{Scheduler, UncaughtExceptionReporter}
import monix.reactive.Observer
import scopt.OParser

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

object Importer extends ScorexLogging {
  //noinspection ScalaStyle
  def main(args: Array[String]): Unit = {
    OParser.parse(commandParser, args, ImportOptions()).foreach {
      case ImportOptions(configFile, blockchainFile, importHeight, format, verifyTransactions) =>
        val config   = loadConfig(ConfigFactory.parseFile(configFile))
        val settings = WavesSettings.fromRootConfig(config)
        AddressScheme.current = new AddressScheme {
          override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
        }

        implicit val scheduler: Scheduler = Scheduler.singleThread("appender")
        val utxPoolStub: UtxPool = new UtxPool {
          override def putIfNew(tx: Transaction, b: Boolean)                               = ???
          override def removeAll(txs: Traversable[Transaction]): Unit          = {}
          override def spendableBalance(addr: Address, assetId: Asset): Long   = ???
          override def pessimisticPortfolio(addr: Address): Portfolio          = ???
          override def all                                                     = ???
          override def size                                                    = ???
          override def transactionById(transactionId: ByteStr)                 = ???
          override def packUnconfirmed(rest: MultiDimensionalMiningConstraint, maxPackTime: Duration): (Seq[Transaction], MultiDimensionalMiningConstraint) = ???
          override def close(): Unit                                           = {}
        }

        val time = new NTP(settings.ntpServer)
        log.info(s"Loading file '$blockchainFile'")

        Try(new FileInputStream(blockchainFile)) match {
          case Success(inputStream) =>
            val db                = openDB(settings.dbSettings.directory)
            val blockchainUpdater = StorageFactory(settings, db, time, Observer.empty(UncaughtExceptionReporter.LogExceptionsToStandardErr))
            val pos               = new PoSSelector(blockchainUpdater, settings.blockchainSettings, settings.synchronizationSettings)
            val extAppender       = BlockAppender(blockchainUpdater, time, utxPoolStub, pos, settings, scheduler, verifyTransactions) _
            checkGenesis(settings, blockchainUpdater)
            val bis           = new BufferedInputStream(inputStream)
            var quit          = false
            val lenBytes      = new Array[Byte](Ints.BYTES)
            val start         = System.currentTimeMillis()
            var counter       = 0
            var blocksToSkip  = blockchainUpdater.height - 1
            val blocksToApply = importHeight - blockchainUpdater.height + 1

            println(s"Skipping $blocksToSkip blocks(s)")

            while (!quit && counter < blocksToApply) {
              val s1 = bis.read(lenBytes)
              if (s1 == Ints.BYTES) {
                val len    = Ints.fromByteArray(lenBytes)
                val buffer = new Array[Byte](len)
                val s2     = bis.read(buffer)
                if (s2 == len) {
                  if (blocksToSkip > 0) {
                    blocksToSkip -= 1
                  } else {
                    val Right(block) =
                      if (format == "BINARY_OLD") Block.parseBytes(buffer).toEither
                      else PBBlocks.vanilla(protobuf.block.PBBlock.parseFrom(buffer), unsafe = true)

                    if (blockchainUpdater.lastBlockId.contains(block.reference)) {
                      Await.result(extAppender.apply(block).runAsync, Duration.Inf) match {
                        case Left(ve) =>
                          log.error(s"Error appending block: $ve")
                          quit = true
                        case _ =>
                          counter = counter + 1
                      }
                    }
                  }
                } else {
                  println(s"$s2 != expected $len")
                  quit = true
                }
              } else {
                println(s"Expecting to read ${Ints.BYTES} but got $s1 (${bis.available()})")
                quit = true
              }
            }
            bis.close()
            inputStream.close()
            val duration = System.currentTimeMillis() - start
            log.info(s"Imported $counter block(s) in ${humanReadableDuration(duration)}")
          case Failure(_) => log.error(s"Failed to open file '$blockchainFile")
        }

        time.close()
    }
  }

  private[this] final case class ImportOptions(configFile: File = new File("waves-testnet.conf"),
                                               blockchainFile: File = new File("blockchain"),
                                               importHeight: Int = Int.MaxValue,
                                               format: String = "BINARY_OLD",
                                               verify: Boolean = true)

  private[this] lazy val commandParser = {
    import scopt.OParser

    val builder = OParser.builder[ImportOptions]
    import builder._

    OParser.sequence(
      programName("waves import"),
      head("Waves Blockchain Importer", Version.VersionString),
      opt[File]('c', "config")
        .text("Config file name")
        .action((f, c) => c.copy(configFile = f)),
      opt[File]('i', "input-file")
        .required()
        .text("Blockchain data file name")
        .action((f, c) => c.copy(blockchainFile = f)),
      opt[Int]('h', "height")
        .text("Import to height")
        .action((h, c) => c.copy(importHeight = h))
        .validate(h => if (h > 0) success else failure("Import height must be > 0")),
      opt[String]('f', "format")
        .text("Blockchain data file format")
        .action((f, c) => c.copy(format = f))
        .valueName("<BINARY|BINARY_OLD> (default is BINARY_OLD)")
        .validate {
          case f if Set("BINARY", "BINARY_OLD").contains(f.toUpperCase) => success
          case f                                                        => failure(s"Unsupported format: $f")
        },
      opt[Unit]('n', "no-verify")
        .text("Disable signatures verification")
        .action((n, c) => c.copy(verify = false)),
      help("help").hidden()
    )
  }
}
