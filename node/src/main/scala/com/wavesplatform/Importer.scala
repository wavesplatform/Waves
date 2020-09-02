package com.wavesplatform

import java.io._
import java.net.{MalformedURLException, URL}

import akka.actor.ActorSystem
import com.google.common.io.ByteStreams
import com.google.common.primitives.Ints
import com.wavesplatform.Exporter.Formats
import com.wavesplatform.account.{Address, AddressScheme}
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi, CommonBlocksApi, CommonTransactionsApi}
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.database.openDB
import com.wavesplatform.events.{BlockchainUpdateTriggersImpl, BlockchainUpdated, UtxEvent}
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.{Blockchain, BlockchainUpdaterImpl, Diff, Height}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{Asset, DiscardedBlocks, Transaction}
import com.wavesplatform.utils._
import com.wavesplatform.utx.{UtxPool, UtxPoolImpl}
import com.wavesplatform.wallet.Wallet
import kamon.Kamon
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.subjects.PublishSubject
import monix.reactive.{Observable, Observer}
import org.iq80.leveldb.DB
import scopt.OParser

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object Importer extends ScorexLogging {
  import monix.execution.Scheduler.Implicits.global

  type AppendBlock = Block => Task[Either[ValidationError, Option[BigInt]]]

  final case class ImportOptions(
      configFile: File = new File("waves-testnet.conf"),
      blockchainFile: String = "blockchain",
      importHeight: Int = Int.MaxValue,
      format: String = Formats.Binary,
      verify: Boolean = true
  )

  def parseOptions(args: Array[String]): ImportOptions = {
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
        opt[String]('i', "input-file")
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
          .action((_, c) => c.copy(verify = false)),
        help("help").hidden()
      )
    }

    OParser
      .parse(commandParser, args, ImportOptions())
      .getOrElse(throw new IllegalArgumentException("Invalid options"))
  }

  def loadSettings(file: File): WavesSettings = Application.loadApplicationConfig(Some(file))

  def initExtensions(
      wavesSettings: WavesSettings,
      blockchainUpdater: BlockchainUpdaterImpl,
      appenderScheduler: Scheduler,
      extensionTime: Time,
      utxPool: UtxPool,
      blockchainUpdatedObservable: Observable[BlockchainUpdated],
      db: DB,
      extensionActorSystem: ActorSystem
  ): Seq[Extension] =
    if (wavesSettings.extensions.isEmpty) Seq.empty
    else {
      val extensionContext: Context = {
        new Context {
          override def settings: WavesSettings = wavesSettings
          override def blockchain: Blockchain  = blockchainUpdater
          override def rollbackTo(blockId: ByteStr): Task[Either[ValidationError, DiscardedBlocks]] =
            Task(blockchainUpdater.removeAfter(blockId)).executeOn(appenderScheduler)
          override def time: Time     = extensionTime
          override def wallet: Wallet = Wallet(settings.walletSettings)
          override def utx: UtxPool   = utxPool

          override def broadcastTransaction(tx: Transaction): TracedResult[ValidationError, Boolean] =
            TracedResult.wrapE(Left(GenericError("Not implemented during import")))
          override def spendableBalanceChanged: Observable[(Address, Asset)] = Observable.empty
          override def actorSystem: ActorSystem                              = extensionActorSystem
          override def blockchainUpdated: Observable[BlockchainUpdated]      = blockchainUpdatedObservable
          override def utxEvents: Observable[UtxEvent]                       = Observable.empty
          override def transactionsApi: CommonTransactionsApi =
            CommonTransactionsApi(
              blockchainUpdater.bestLiquidDiff.map(diff => Height(blockchainUpdater.height) -> diff),
              db,
              blockchainUpdater,
              utxPool,
              wallet,
              _ => TracedResult.wrapE(Left(GenericError("Not implemented during import"))),
              Application.loadBlockAt(db, blockchainUpdater)
            )
          override def blocksApi: CommonBlocksApi =
            CommonBlocksApi(blockchainUpdater, Application.loadBlockMetaAt(db, blockchainUpdater), Application.loadBlockInfoAt(db, blockchainUpdater))
          override def accountsApi: CommonAccountsApi =
            CommonAccountsApi(blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), db, blockchainUpdater)
          override def assetsApi: CommonAssetsApi =
            CommonAssetsApi(blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), db, blockchainUpdater)
        }
      }

      val extensions = wavesSettings.extensions.map { extensionClassName =>
        val extensionClass = Class.forName(extensionClassName).asInstanceOf[Class[Extension]]
        val ctor           = extensionClass.getConstructor(classOf[Context])
        log.info(s"Enable extension: $extensionClassName")
        ctor.newInstance(extensionContext)
      }
      extensions.flatMap { ext =>
        Try(ext.start()) match {
          case Success(_) =>
            Some(ext)
          case Failure(e) =>
            log.warn(s"Can't initialize extension $ext", e)
            None
        }
      }
    }

  @volatile private var quit = false
  private val lock           = new Object

  def startImport(bis: BufferedInputStream, blockchain: Blockchain, appendBlock: AppendBlock, importOptions: ImportOptions): Unit = {
    val lenBytes = new Array[Byte](Ints.BYTES)
    val start    = System.nanoTime()
    var counter  = 0

    val startHeight   = blockchain.height
    var blocksToSkip  = startHeight - 1
    val blocksToApply = importOptions.importHeight - startHeight + 1

    log.info(s"Skipping $blocksToSkip block(s)")

    sys.addShutdownHook {
      import scala.concurrent.duration._
      val millis = (System.nanoTime() - start).nanos.toMillis
      log.info(s"Imported $counter block(s) from $startHeight to ${startHeight + counter} in ${humanReadableDuration(millis)}")
    }

    while (!quit && counter < blocksToApply) lock.synchronized {
      val s1 = ByteStreams.read(bis, lenBytes, 0, Ints.BYTES)
      if (s1 == Ints.BYTES) {
        val len    = Ints.fromByteArray(lenBytes)
        val buffer = new Array[Byte](len)
        val s2     = ByteStreams.read(bis, buffer, 0, len)
        if (s2 == len) {
          if (blocksToSkip > 0) {
            blocksToSkip -= 1
          } else {
            val blockV5 = blockchain.isFeatureActivated(
              BlockchainFeatures.BlockV5,
              blockchain.height + 1
            )
            val block =
              (if (importOptions.format == Formats.Binary && !blockV5) Block.parseBytes(buffer)
               else PBBlocks.vanilla(PBBlocks.addChainId(protobuf.block.PBBlock.parseFrom(buffer)), unsafe = true)).get
            if (blockchain.lastBlockId.contains(block.header.reference)) {
              Await.result(appendBlock(block).runAsyncLogErr, Duration.Inf) match {
                case Left(ve) =>
                  log.error(s"Error appending block: $ve")
                  quit = true
                case _ =>
                  counter = counter + 1
              }
            }
          }
        } else {
          log.info(s"$s2 != expected $len")
          quit = true
        }
      } else {
        if (bis.available() > 0) log.info(s"Expecting to read ${Ints.BYTES} but got $s1 (${bis.available()})")
        quit = true
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val importOptions = parseOptions(args)
    val settings      = loadSettings(importOptions.configFile)
    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    def initFileStream(file: String): InputStream = {
      log.info(s"Opening import file: $file")
      file match {
        case "-" =>
          System.in

        case _ =>
          System.setProperty("http.agent", s"waves-node/${Version.VersionString}")
          try new URL(file).openStream()
          catch {
            case _: MalformedURLException =>
              new FileInputStream(file)
          }
      }
    }

    val bis = new BufferedInputStream(initFileStream(importOptions.blockchainFile), 2 * 1024 * 1024)

    val scheduler = Schedulers.singleThread("appender")
    val time      = new NTP(settings.ntpServer)

    val actorSystem              = ActorSystem("wavesplatform-import")
    val blockchainUpdated        = PublishSubject[BlockchainUpdated]()
    val blockchainUpdateTriggers = new BlockchainUpdateTriggersImpl(blockchainUpdated)
    val db                       = openDB(settings.dbSettings.directory)
    val (blockchainUpdater, levelDb) =
      StorageFactory(settings, db, time, Observer.empty, blockchainUpdateTriggers)
    val utxPool     = new UtxPoolImpl(time, blockchainUpdater, PublishSubject(), settings.utxSettings)
    val pos         = PoSSelector(blockchainUpdater, settings.synchronizationSettings)
    val extAppender = BlockAppender(blockchainUpdater, time, utxPool, pos, scheduler, importOptions.verify) _

    checkGenesis(settings, blockchainUpdater)
    val extensions = initExtensions(settings, blockchainUpdater, scheduler, time, utxPool, blockchainUpdated, db, actorSystem)

    sys.addShutdownHook {
      quit = true
      Await.ready(Future.sequence(extensions.map(_.shutdown())), settings.extensionsShutdownTimeout)
      Await.result(actorSystem.terminate(), 10.second)
      lock.synchronized {
        if (blockchainUpdater.isFeatureActivated(BlockchainFeatures.NG)) {
          // Force store liquid block in leveldb
          val lastHeader = blockchainUpdater.lastBlockHeader.get.header
          val pseudoBlock = Block(
            BlockHeader(
              blockchainUpdater.blockVersionAt(blockchainUpdater.height),
              System.currentTimeMillis(),
              blockchainUpdater.lastBlockId.get,
              lastHeader.baseTarget,
              lastHeader.generationSignature,
              lastHeader.generator,
              Nil,
              0,
              ByteStr.empty
            ),
            ByteStr.empty,
            Nil
          )
          blockchainUpdater.processBlock(pseudoBlock, ByteStr.empty, verify = false)
        }
        scheduler.shutdown()
        blockchainUpdater.shutdown()
        levelDb.close()
        db.close()
      }
    }

    startImport(bis, blockchainUpdater, extAppender, importOptions)
    Await.result(Kamon.stopModules(), 10.seconds)
  }
}
