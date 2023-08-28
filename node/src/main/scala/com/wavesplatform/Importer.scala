package com.wavesplatform

import akka.actor.ActorSystem
import cats.implicits.catsSyntaxOption
import com.google.common.io.ByteStreams
import com.google.common.primitives.Ints
import com.wavesplatform.Exporter.Formats
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi, CommonBlocksApi, CommonTransactionsApi}
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.database.{DBExt, KeyTags, RDB}
import com.wavesplatform.events.{BlockchainUpdateTriggers, UtxEvent}
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.{BlockChallenger, Miner}
import com.wavesplatform.protobuf.block.{PBBlocks, VanillaBlock}
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.{Blockchain, BlockchainUpdaterImpl, Height, ParSignatureChecker}
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import com.wavesplatform.state.ParSignatureChecker.sigverify
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{DiscardedBlocks, Transaction}
import com.wavesplatform.utils.*
import com.wavesplatform.utx.{UtxPool, UtxPoolImpl}
import com.wavesplatform.wallet.Wallet
import kamon.Kamon
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scopt.OParser

import java.io.*
import java.net.{MalformedURLException, URL}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object Importer extends ScorexLogging {

  type AppendBlock = Block => Task[Either[ValidationError, BlockApplyResult]]

  final case class ImportOptions(
      configFile: Option[File] = None,
      blockchainFile: String = "blockchain",
      importHeight: Int = Int.MaxValue,
      format: String = Formats.Binary,
      verify: Boolean = true,
      dryRun: Boolean = false,
      maxQueueSize: Int = 100
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
        opt[Int]('q', "max-queue-size")
          .text("Max size of blocks' queue")
          .action((maxSize, c) => c.copy(maxQueueSize = maxSize))
          .validate(maxSize => if (maxSize > 0) success else failure("Max blocks' queue size must be > 0")),
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
      rdb: RDB,
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
          override def actorSystem: ActorSystem        = extensionActorSystem
          override def utxEvents: Observable[UtxEvent] = Observable.empty
          override def transactionsApi: CommonTransactionsApi =
            CommonTransactionsApi(
              blockchainUpdater.bestLiquidSnapshot.map(Height(blockchainUpdater.height) -> _),
              rdb,
              blockchainUpdater,
              utxPool,
              BlockChallenger.NoOp,
              _ => Future.successful(TracedResult.wrapE(Left(GenericError("Not implemented during import")))),
              Application.loadBlockAt(rdb, blockchainUpdater)
            )
          override def blocksApi: CommonBlocksApi =
            CommonBlocksApi(
              blockchainUpdater,
              Application.loadBlockMetaAt(rdb.db, blockchainUpdater),
              Application.loadBlockInfoAt(rdb, blockchainUpdater)
            )
          override def accountsApi: CommonAccountsApi =
            CommonAccountsApi(() => blockchainUpdater.snapshotBlockchain, rdb, blockchainUpdater)
          override def assetsApi: CommonAssetsApi =
            CommonAssetsApi(() => blockchainUpdater.bestLiquidSnapshot.orEmpty, rdb.db, blockchainUpdater)
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
      skipBlocks: Boolean,
      appender: Scheduler
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

    val maxSize = importOptions.maxQueueSize
    val queue   = new mutable.Queue[VanillaBlock](maxSize)

    @tailrec
    def readBlocks(queue: mutable.Queue[VanillaBlock], remainCount: Int, maxCount: Int): Unit = {
      if (remainCount == 0) ()
      else {
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
              val blockV5               = blockchain.isFeatureActivated(BlockchainFeatures.BlockV5, blockchain.height + (maxCount - remainCount) + 1)
              val rideV6                = blockchain.isFeatureActivated(BlockchainFeatures.RideV6, blockchain.height + (maxCount - remainCount) + 1)
              lazy val parsedProtoBlock = PBBlocks.vanilla(PBBlocks.addChainId(protobuf.block.PBBlock.parseFrom(blockBytes)), unsafe = true)

              val block = (if (!blockV5) Block.parseBytes(blockBytes) else parsedProtoBlock).orElse(parsedProtoBlock).get

              ParSignatureChecker.checkBlockAndTxSignatures(block, rideV6)

              queue.enqueue(block)
            }
            readBlocks(queue, remainCount - 1, maxCount)
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

    while ((!quit || queue.nonEmpty) && counter < blocksToApply)
      if (!quit && queue.isEmpty) {
        readBlocks(queue, maxSize, maxSize)
      } else {
        lock.synchronized {
          val block = queue.dequeue()
          if (blockchain.lastBlockId.contains(block.header.reference)) {
            Await.result(appendBlock(block).runAsyncLogErr(appender), Duration.Inf) match {
              case Left(ve) =>
                log.error(s"Error appending block: $ve")
                queue.clear()
                quit = true
              case _ =>
                counter = counter + 1
            }
          } else {
            log.warn(s"Block $block is not a child of the last block ${blockchain.lastBlockId.get}")
          }
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
    val rdb         = RDB.open(settings.dbSettings)
    val (blockchainUpdater, _) =
      StorageFactory(settings, rdb, time, BlockchainUpdateTriggers.combined(triggers))
    val utxPool     = new UtxPoolImpl(time, blockchainUpdater, settings.utxSettings, settings.maxTxErrorLogSize, settings.minerSettings.enable)
    val pos         = PoSSelector(blockchainUpdater, settings.synchronizationSettings.maxBaseTarget)
    val extAppender = BlockAppender(blockchainUpdater, time, _ => (), pos, scheduler, importOptions.verify, txSignParCheck = false) _

    val extensions = initExtensions(settings, blockchainUpdater, scheduler, time, utxPool, rdb, actorSystem)
    checkGenesis(settings, blockchainUpdater, Miner.Disabled)

    val importFileOffset =
      if (importOptions.dryRun) 0
      else
        importOptions.format match {
          case Formats.Binary =>
            var result = 0L
            rdb.db.iterateOver(KeyTags.BlockInfoAtHeight) { e =>
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
          // Force store liquid block in rocksdb
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
              ByteStr.empty,
              None,
              None
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

        utxPool.close()
        blockchainUpdater.shutdown()
        rdb.close()
      }
      inputStream.close()
    }

    startImport(
      inputStream,
      blockchainUpdater,
      extAppender,
      importOptions,
      importFileOffset == 0,
      scheduler
    )
    Await.result(Kamon.stopModules(), 10.seconds)
  }
}
