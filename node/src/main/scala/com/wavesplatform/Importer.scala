package com.wavesplatform

import akka.actor.ActorSystem
import com.google.common.io.ByteStreams
import com.google.common.primitives.Ints
import com.wavesplatform.Exporter.Formats
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi, CommonBlocksApi, CommonTransactionsApi}
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.database.{DBExt, KeyTags, openDB}
import com.wavesplatform.events.{BlockchainUpdateTriggers, UtxEvent}
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.Miner
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.{Blockchain, BlockchainUpdaterImpl, Diff, Height}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{Asset, DiscardedBlocks, Transaction}
import com.wavesplatform.utils.*
import com.wavesplatform.utx.{UtxPool, UtxPoolImpl}
import com.wavesplatform.wallet.Wallet
import kamon.Kamon
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.{Observable, Observer}
import org.iq80.leveldb.DB
import scopt.OParser

import java.io.*
import java.net.{MalformedURLException, URL}
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object Importer extends ScorexLogging {
  import monix.execution.Scheduler.Implicits.global

  type AppendBlock = Block => Task[Either[ValidationError, BlockApplyResult]]

  final case class ImportOptions(
      configFile: Option[File] = None,
      blockchainFile: String = "blockchain",
      importHeight: Int = Int.MaxValue,
      format: String = Formats.Binary,
      verify: Boolean = true,
      dryRun: Boolean = false
  )

  def parseOptions(args: Array[String]): ImportOptions = {
    lazy val commandParser = {
      import scopt.OParser

      val builder = OParser.builder[ImportOptions]
      import builder.*

      OParser.sequence(
        programName("waves import"),
        head("Waves Blockchain Importer", Version.VersionString),
        opt[File]('c', "config")
          .text("Config file name")
          .action((f, c) => c.copy(configFile = Some(f))),
        opt[String]('i', "input-file")
          .required()
          .text("Blockchain data file name")
          .action((f, c) => c.copy(blockchainFile = f)),
        opt[Int]('h', "height")
          .text("Import to height")
          .action((h, c) => c.copy(importHeight = h))
          .validate(h => if (h > 0) success else failure("Import height must be > 0")),
        opt[String]('f', "format")
          .hidden()
          .text("Blockchain data file format")
          .action((f, c) => c.copy(format = f))
          .valueName(s"<${Formats.importerList.mkString("|")}> (default is ${Formats.default})")
          .validate {
            case f if Formats.isSupportedInImporter(f) => success
            case f                                     => failure(s"Unsupported format: $f")
          },
        opt[Unit]("dry-run").action((_, c) => c.copy(dryRun = true)),
        opt[Unit]('n', "no-verify")
          .text("Disable signatures verification")
          .action((_, c) => c.copy(verify = false)),
        help("help").hidden()
      )
    }

    OParser
      .parse(commandParser, args, ImportOptions())
      .getOrElse {
        println(OParser.usage(commandParser))
        sys.exit(1)
      }
  }

  def loadSettings(file: Option[File]): WavesSettings = Application.loadApplicationConfig(file)

  private[this] var triggers = Seq.empty[BlockchainUpdateTriggers]

  def initExtensions(
      wavesSettings: WavesSettings,
      blockchainUpdater: BlockchainUpdaterImpl,
      appenderScheduler: Scheduler,
      extensionTime: Time,
      utxPool: UtxPool,
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
          override def utxEvents: Observable[UtxEvent]                       = Observable.empty
          override def transactionsApi: CommonTransactionsApi =
            CommonTransactionsApi(
              blockchainUpdater.bestLiquidDiff.map(diff => Height(blockchainUpdater.height) -> diff),
              db,
              blockchainUpdater,
              utxPool,
              _ => Future.successful(TracedResult.wrapE(Left(GenericError("Not implemented during import")))),
              Application.loadBlockAt(db, blockchainUpdater)
            )
          override def blocksApi: CommonBlocksApi =
            CommonBlocksApi(blockchainUpdater, Application.loadBlockMetaAt(db, blockchainUpdater), Application.loadBlockInfoAt(db, blockchainUpdater))
          override def accountsApi: CommonAccountsApi =
            CommonAccountsApi(() => blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), db, blockchainUpdater)
          override def assetsApi: CommonAssetsApi =
            CommonAssetsApi(() => blockchainUpdater.bestLiquidDiff.getOrElse(Diff.empty), db, blockchainUpdater)
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
            triggers ++= Some(ext).collect { case t: BlockchainUpdateTriggers => t }
            Some(ext)
          case Failure(e) =>
            log.warn(s"Can't initialize extension $ext", e)
            None
        }
      }
    }

  @volatile private var quit = false
  private val lock           = new Object

  // noinspection UnstableApiUsage
  def startImport(
      inputStream: BufferedInputStream,
      blockchain: Blockchain,
      appendBlock: AppendBlock,
      importOptions: ImportOptions,
      skipBlocks: Boolean
  ): Unit = {
    val lenBytes = new Array[Byte](Ints.BYTES)
    val start    = System.nanoTime()
    var counter  = 0

    val startHeight   = blockchain.height
    var blocksToSkip  = if (skipBlocks) startHeight - 1 else 0
    val blocksToApply = importOptions.importHeight - startHeight + 1

    if (blocksToSkip > 0) log.info(s"Skipping $blocksToSkip block(s)")

    sys.addShutdownHook {
      import scala.concurrent.duration.*
      val millis = (System.nanoTime() - start).nanos.toMillis
      log.info(
        s"Imported $counter block(s) from $startHeight to ${startHeight + counter} in ${humanReadableDuration(millis)}"
      )
    }

    while (!quit && counter < blocksToApply) lock.synchronized {
      val s1 = ByteStreams.read(inputStream, lenBytes, 0, Ints.BYTES)
      if (s1 == Ints.BYTES) {
        val blockSize = Ints.fromByteArray(lenBytes)

        lazy val blockBytes = new Array[Byte](blockSize)
        val factReadSize =
          if (blocksToSkip > 0) {
            // File IO optimization
            ByteStreams.skipFully(inputStream, blockSize)
            blockSize
          } else {
            ByteStreams.read(inputStream, blockBytes, 0, blockSize)
          }

        if (factReadSize == blockSize) {
          if (blocksToSkip > 0) {
            blocksToSkip -= 1
          } else {
            val blockV5 = blockchain.isFeatureActivated(
              BlockchainFeatures.BlockV5,
              blockchain.height + 1
            )
            val block =
              (if (importOptions.format == Formats.Binary && !blockV5) Block.parseBytes(blockBytes)
               else PBBlocks.vanilla(PBBlocks.addChainId(protobuf.block.PBBlock.parseFrom(blockBytes)), unsafe = true)).get
            if (blockchain.lastBlockId.contains(block.header.reference)) {
              Await.result(appendBlock(block).runAsyncLogErr, Duration.Inf) match {
                case Left(ve) =>
                  log.error(s"Error appending block: $ve")
                  quit = true
                case _ =>
                  counter = counter + 1
              }
            } else {
              log.warn(s"Block $block is not a child of the last block ${blockchain.lastBlockId.get}")
            }
          }
        } else {
          log.info(s"$factReadSize != expected $blockSize")
          quit = true
        }
      } else {
        if (inputStream.available() > 0) log.info(s"Expecting to read ${Ints.BYTES} but got $s1 (${inputStream.available()})")
        quit = true
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val importOptions = parseOptions(args)
    val settings      = loadSettings(importOptions.configFile)

    def initFileStream(file: String, offset: Long): InputStream = {
      log.info(s"Opening import file: $file, offset=$offset")
      file match {
        case "-" =>
          System.in

        case _ =>
          System.setProperty("http.agent", s"waves-node/${Version.VersionString}")
          try {
            val url        = new URL(file)
            val connection = url.openConnection()
            if (offset > 0) connection.setRequestProperty("Range", s"bytes=$offset-")
            connection.connect()
            connection.getInputStream
          } catch {
            case _: MalformedURLException =>
              val fs = new FileInputStream(file)
              if (offset > 0) fs.skip(offset)
              fs
          }
      }
    }

    val scheduler = Schedulers.singleThread("appender")
    val time      = new NTP(settings.ntpServer)

    val actorSystem = ActorSystem("wavesplatform-import")
    val db          = openDB(settings.dbSettings.directory)
    val (blockchainUpdater, levelDb) =
      StorageFactory(settings, db, time, Observer.empty, BlockchainUpdateTriggers.combined(triggers))
    val utxPool     = new UtxPoolImpl(time, blockchainUpdater, settings.utxSettings, settings.maxTxErrorLogSize, settings.minerSettings.enable)
    val pos         = PoSSelector(blockchainUpdater, settings.synchronizationSettings.maxBaseTarget)
    val extAppender = BlockAppender(blockchainUpdater, time, utxPool, pos, scheduler, importOptions.verify) _

    val extensions = initExtensions(settings, blockchainUpdater, scheduler, time, utxPool, db, actorSystem)
    checkGenesis(settings, blockchainUpdater, Miner.Disabled)

    val importFileOffset =
      if (importOptions.dryRun) 0
      else
        importOptions.format match {
          case Formats.Binary =>
            var result = 0L
            db.iterateOver(KeyTags.BlockInfoAtHeight) { e =>
              e.getKey match {
                case Array(_, _, 0, 0, 0, 1) => // Skip genesis
                case _ =>
                  val meta = com.wavesplatform.database.readBlockMeta(e.getValue)
                  result += meta.size + 4
              }
            }
            result

          case _ => 0L
        }
    val inputStream = new BufferedInputStream(initFileStream(importOptions.blockchainFile, importFileOffset), 2 * 1024 * 1024)

    if (importOptions.dryRun) {
      def readNextBlock(): Future[Option[Block]] = Future.successful(None)
      readNextBlock().flatMap {
        case None =>
          Future.successful(())

        case Some(_) =>
          readNextBlock()
      }
    }

    sys.addShutdownHook {
      quit = true
      Await.result(actorSystem.terminate(), 10.second)
      lock.synchronized {
        if (blockchainUpdater.isFeatureActivated(BlockchainFeatures.NG) && blockchainUpdater.liquidBlockMeta.nonEmpty) {
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

        // Terminate appender
        scheduler.shutdown()
        scheduler.awaitTermination(10 seconds)

        // Terminate extensions
        Await.ready(Future.sequence(extensions.map(_.shutdown())), settings.extensionsShutdownTimeout)

        blockchainUpdater.shutdown()
        levelDb.close()
        db.close()
      }
      inputStream.close()
    }

    startImport(inputStream, blockchainUpdater, extAppender, importOptions, importFileOffset == 0)
    Await.result(Kamon.stopModules(), 10.seconds)
  }
}
