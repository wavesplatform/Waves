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
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.{Blockchain, BlockchainUpdated, Portfolio}
import com.wavesplatform.transaction.{Asset, Transaction}
import com.wavesplatform.utils._
import com.wavesplatform.utx.UtxPool
import monix.eval.Task
import monix.execution.{Scheduler, UncaughtExceptionReporter}
import monix.reactive.Observer

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

object Importer extends ScorexLogging {
  private implicit class TryWithTap[T](t: Try[T]) {
    def tapLeft(cb: Throwable => Unit): Try[T] = {
      t match {
        case Failure(throwable) => cb(throwable)
        case _                  => ()
      }
      t
    }
  }

  type AppendBlock = Block => Task[Either[ValidationError, Option[BigInt]]]

  case class ImporterSettings(
      verifyTransactions: Boolean,
      configFilename: String,
      blockchainFilename: String,
      importHeight: Int,
      format: String
  )

  def parseArgs(args: Array[String]): Try[ImporterSettings] = {
    val argi = args.iterator
    val (verifyTransactions, configOpt) = {
      Try(argi.next) match {
        case Success("-n") | Success("-no-verify") => (false, Try(argi.next))
        case conf                                  => (true, conf)
      }
    }
    val configFilename     = configOpt.toOption.getOrElse("waves-testnet.conf")
    val blockchainFilename = Try(argi.next).tapLeft(_ => log.error("Usage: Importer [-n | -no-verify] <config file> <blockchain file> [height]"))
    val importHeight       = Try(argi.next).map(_.toInt).getOrElse(Int.MaxValue)

    val format = Try(argi.next)
      .map(_.toUpperCase)
      .collect { case custom @ "BINARY" => custom }
      .getOrElse("BINARY_OLD")
      .intern()

    blockchainFilename
      .map(ImporterSettings(verifyTransactions, configFilename, _, importHeight, format))
  }

  def loadSettings(fileName: String): WavesSettings =
    ((name: String) => new File(name)) andThen
      ConfigFactory.parseFile andThen
      settings.loadConfig andThen
      WavesSettings.fromRootConfig apply fileName

  def initFileStream(fileName: String): Try[FileInputStream] =
    Try(new FileInputStream(fileName))
      .tapLeft(_ => log.error(s"Failed to open file '$fileName"))

  def initTime(ntpServer: String): NTP = new NTP(ntpServer)

  def initUtxPool(): UtxPool = new UtxPool {
    override def putIfNew(tx: Transaction, b: Boolean)           = ???
    override def removeAll(txs: Traversable[Transaction]): Unit          = {}
    override def spendableBalance(addr: Address, assetId: Asset): Long   = ???
    override def pessimisticPortfolio(addr: Address): Portfolio          = ???
    override def all                                                     = ???
    override def size                                                    = ???
    override def transactionById(transactionId: ByteStr)                 = ???
    override def packUnconfirmed(rest: MultiDimensionalMiningConstraint) = ???
    override def close(): Unit                                           = {}
  }

  def initBlockchain(scheduler: Scheduler,
                     time: NTP,
                     utxPool: UtxPool,
                     settings: WavesSettings,
                     importerSettings: ImporterSettings,
                     blockchainUpdated: Option[Observer[BlockchainUpdated]]): (Blockchain, AppendBlock) = {
    val db = openDB(settings.dbSettings.directory)
    val blockchainUpdater =
      StorageFactory(settings,
                     db,
                     time,
                     Observer.empty(UncaughtExceptionReporter.LogExceptionsToStandardErr),
                     blockchainUpdated)
    val pos         = new PoSSelector(blockchainUpdater, settings.blockchainSettings, settings.synchronizationSettings)
    val extAppender = BlockAppender(blockchainUpdater, time, utxPool, pos, settings, scheduler, importerSettings.verifyTransactions) _

    checkGenesis(settings, blockchainUpdater)

    (blockchainUpdater, extAppender)
  }

  def startImport(scheduler: Scheduler,
                  bis: BufferedInputStream,
                  blockchainUpdater: Blockchain,
                  appendBlock: AppendBlock,
                  importerSettings: ImporterSettings): Unit = {
    var quit     = false
    val lenBytes = new Array[Byte](Ints.BYTES)
    val start    = System.currentTimeMillis()
    var counter  = 0

    var blocksToSkip  = blockchainUpdater.height - 1
    val blocksToApply = importerSettings.importHeight - blockchainUpdater.height + 1

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
              if (importerSettings.format == "BINARY_OLD") Block.parseBytes(buffer).toEither
              else PBBlocks.vanilla(protobuf.block.PBBlock.parseFrom(buffer), unsafe = true)

            if (blockchainUpdater.lastBlockId.contains(block.reference)) {
              Await.result(appendBlock(block).runAsync(scheduler), Duration.Inf) match {
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

    val duration = System.currentTimeMillis() - start
    log.info(s"Imported $counter block(s) in ${humanReadableDuration(duration)}")
  }

  def run(args: Array[String]): Try[() => Unit] =
    for {
      importerSettings <- parseArgs(args)
      wavesSettings = loadSettings(importerSettings.configFilename)
      _ = AddressScheme.current = new AddressScheme {
        override val chainId: Byte = wavesSettings.blockchainSettings.addressSchemeCharacter.toByte
      }

      fis <- initFileStream(importerSettings.blockchainFilename)
      bis = new BufferedInputStream(fis)

      scheduler                        = Scheduler.singleThread("appender")
      time                             = initTime(wavesSettings.ntpServer)
      utxPool                          = initUtxPool()
      (blockchainUpdater, appendBlock) = initBlockchain(scheduler, time, utxPool, wavesSettings, importerSettings, None)
      _                                = startImport(scheduler, bis, blockchainUpdater, appendBlock, importerSettings)
    } yield
      () => {
        bis.close()
        fis.close()
        time.close()
        utxPool.close()
      }

  def main(args: Array[String]): Unit = {
    run(args) match {
      case Success(shutdown) => shutdown()
      case Failure(ext)      => log.error(ext.getMessage)
    }
  }

}
