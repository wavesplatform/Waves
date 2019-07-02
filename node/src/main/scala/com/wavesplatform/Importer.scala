package com.wavesplatform

import java.io._

import akka.actor.ActorSystem
import com.google.common.primitives.Ints
import com.wavesplatform.Exporter.Formats
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.db.openDB
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MultiDimensionalMiningConstraint
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.{Blockchain, BlockchainUpdated, Portfolio}
import com.wavesplatform.transaction.{Asset, BlockchainUpdater, DiscardedBlocks, Transaction}
import com.wavesplatform.utils._
import com.wavesplatform.utx.UtxPool
import com.wavesplatform.wallet.Wallet
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.subjects.ConcurrentSubject
import monix.reactive.{Observable, Observer}
import scopt.OParser

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object Importer extends ScorexLogging {
  import monix.execution.Scheduler.Implicits.global

  type AppendBlock = Block => Task[Either[ValidationError, Option[BigInt]]]

  final case class ImportOptions(configFile: File = new File("waves-testnet.conf"),
                                 blockchainFile: File = new File("blockchain"),
                                 importHeight: Int = Int.MaxValue,
                                 format: String = Formats.Binary,
                                 verify: Boolean = true)

  def parseOptions(args: Array[String]): Try[ImportOptions] = {
    lazy val commandParser = {
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
          .valueName(s"<${Formats.importerList.mkString("|")}> (default is ${Formats.default})")
          .validate {
            case f if Formats.isSupportedInImporter(f) => success
            case f                                     => failure(s"Unsupported format: $f")
          },
        opt[Unit]('n', "no-verify")
          .text("Disable signatures verification")
          .action((n, c) => c.copy(verify = false)),
        help("help").hidden()
      )
    }

    OParser.parse(commandParser, args, ImportOptions()) match {
      case Some(v) => Success(v)
      case None    => Failure(new IllegalArgumentException("Incorrect arguments"))
    }
  }

  def loadSettings(file: File): WavesSettings = Application.loadApplicationConfig(Some(file))

  def initFileStream(file: File): Try[FileInputStream] =
    Try(new FileInputStream(file)) match {
      case t: Failure[FileInputStream] =>
        log.error(s"Failed to open file '$file")
        t
      case t => t
    }

  def initTime(ntpServer: String): NTP = new NTP(ntpServer)

  def initUtxPool(): UtxPool = new UtxPool {
    override def putIfNew(tx: Transaction, b: Boolean)                 = ???
    override def removeAll(txs: Traversable[Transaction]): Unit        = {}
    override def spendableBalance(addr: Address, assetId: Asset): Long = ???
    override def pessimisticPortfolio(addr: Address): Portfolio        = ???
    override def all                                                   = ???
    override def size                                                  = ???
    override def transactionById(transactionId: ByteStr)               = ???
    override def packUnconfirmed(rest: MultiDimensionalMiningConstraint,
                                 maxPackTime: Duration): (Seq[Transaction], MultiDimensionalMiningConstraint) = ???
    override def close(): Unit                                                                                = {}
  }

  def initBlockchain(scheduler: Scheduler,
                     time: NTP,
                     utxPool: UtxPool,
                     settings: WavesSettings,
                     importOptions: ImportOptions,
                     blockchainUpdated: Observer[BlockchainUpdated]): (Blockchain with BlockchainUpdater, AppendBlock) = {
    val db = openDB(settings.dbSettings.directory)
    val blockchainUpdater =
      StorageFactory(settings, db, time, Observer.empty, blockchainUpdated)
    val pos         = new PoSSelector(blockchainUpdater, settings.blockchainSettings, settings.synchronizationSettings)
    val extAppender = BlockAppender(blockchainUpdater, time, utxPool, pos, settings, scheduler, importOptions.verify) _

    checkGenesis(settings, blockchainUpdater)

    (blockchainUpdater, extAppender)
  }

  def initExtensions(wavesSettings: WavesSettings,
                     blockchainUpdater: Blockchain with BlockchainUpdater,
                     appenderScheduler: Scheduler,
                     time: Time,
                     utxPool: UtxPool,
                     blockchainUpdatedObservable: Observable[BlockchainUpdated]): Seq[Extension] = {
    val extensionContext = {
      val t = time
      new Context {
        override def settings: WavesSettings = wavesSettings
        override def blockchain: Blockchain  = blockchainUpdater
        override def rollbackTo(blockId: ByteStr): Task[Either[ValidationError, DiscardedBlocks]] =
          Task(blockchainUpdater.removeAfter(blockId)).executeOn(appenderScheduler)
        override def time: Time                                            = t
        override def wallet: Wallet                                        = ???
        override def utx: UtxPool                                          = utxPool
        override def broadcastTx(tx: Transaction): Unit                    = ???
        override def spendableBalanceChanged: Observable[(Address, Asset)] = ???
        override def actorSystem: ActorSystem                              = ???
        override def blockchainUpdated: Observable[BlockchainUpdated]      = blockchainUpdatedObservable
      }
    }

    val extensions = wavesSettings.extensions.map { extensionClassName =>
      val extensionClass = Class.forName(extensionClassName).asInstanceOf[Class[Extension]]
      val ctor           = extensionClass.getConstructor(classOf[Context])
      log.info(s"Enable extension: $extensionClassName")
      ctor.newInstance(extensionContext)
    }
    extensions.foreach(_.start())

    extensions
  }

  def startImport(scheduler: Scheduler,
                  bis: BufferedInputStream,
                  blockchainUpdater: Blockchain,
                  appendBlock: AppendBlock,
                  importOptions: ImportOptions): Unit = {
    var quit     = false
    val lenBytes = new Array[Byte](Ints.BYTES)
    val start    = System.currentTimeMillis()
    var counter  = 0

    var blocksToSkip  = blockchainUpdater.height - 1
    val blocksToApply = importOptions.importHeight - blockchainUpdater.height + 1

    println(s"Skipping $blocksToSkip block(s)")

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
            val block = Block.parseBytes(buffer).get

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
      importOptions <- parseOptions(args)
      wavesSettings = loadSettings(importOptions.configFile)
      _ = AddressScheme.current = new AddressScheme {
        override val chainId: Byte = wavesSettings.blockchainSettings.addressSchemeCharacter.toByte
      }

      fis <- initFileStream(importOptions.blockchainFile)
      bis = new BufferedInputStream(fis)

      scheduler                        = Schedulers.singleThread("appender")
      time                             = initTime(wavesSettings.ntpServer)
      utxPool                          = initUtxPool()
      blockchainUpdated                = ConcurrentSubject.publish[BlockchainUpdated]
      (blockchainUpdater, appendBlock) = initBlockchain(scheduler, time, utxPool, wavesSettings, importOptions, blockchainUpdated)
      extensions                       = initExtensions(wavesSettings, blockchainUpdater, scheduler, time, utxPool, blockchainUpdated)
      _                                = startImport(scheduler, bis, blockchainUpdater, appendBlock, importOptions)
    } yield
      () => {
        Await.ready(Future.sequence(extensions.map(_.shutdown())), wavesSettings.extensionsShutdownTimeout)
        bis.close()
        fis.close()
        time.close()
        utxPool.close()
        blockchainUpdated.onComplete()
        blockchainUpdater.shutdown()
      }

  def main(args: Array[String]): Unit = {
    run(args) match {
      case Success(shutdown) => shutdown()
      case Failure(ext)      => log.error(ext.getMessage)
    }
  }
}
