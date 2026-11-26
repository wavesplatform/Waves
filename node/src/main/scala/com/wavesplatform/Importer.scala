package com.wavesplatform

import cats.implicits.catsSyntaxOption
import cats.syntax.apply.*
import com.google.common.io.ByteStreams
import com.google.common.primitives.{Ints, Longs}
import com.wavesplatform.Exporter.Formats
import com.wavesplatform.api.common.*
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.consensus.{CachedVrf, PoSSelector}
import com.wavesplatform.database.{DBExt, RDB}
import com.wavesplatform.events.{BlockchainUpdateTriggers, UtxEvent}
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.evaluator.ScopeEvaluator
import com.wavesplatform.metrics.ImportProbe
import com.wavesplatform.mining.Miner
import com.wavesplatform.network.BlockSnapshotResponse
import com.wavesplatform.protobuf.block.{PBBlocks, VanillaBlock}
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import com.wavesplatform.state.ParSignatureChecker.sigverify
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.{BlockEndorser, Blockchain, BlockchainUpdaterImpl, Height, ParSignatureChecker}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{DiscardedBlocks, Transaction}
import com.wavesplatform.utils.*
import com.wavesplatform.utx.{UtxPool, UtxPoolImpl}
import com.wavesplatform.wallet.Wallet
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import scopt.OParser

import java.io.*
import java.net.URI
import java.time
import java.util.Locale
import java.util.concurrent.ArrayBlockingQueue
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object Importer extends ScorexLogging {

  type AppendBlock = (Block, Option[BlockSnapshotResponse]) => Task[Either[ValidationError, BlockApplyResult]]

  final case class ImportOptions(
      configFile: Option[File] = None,
      blockchainFile: String = "blockchain",
      snapshotsFile: Option[String] = None,
      importHeight: Int = Int.MaxValue,
      format: String = Formats.Binary,
      verify: Boolean = true,
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
        opt[String]('s', "snapshots-file")
          .text("Snapshots data file name")
          .action((f, c) => c.copy(snapshotsFile = Some(f))),
        opt[Int]('h', "height")
          .text("Import to height")
          .action((h, c) => c.copy(importHeight = h))
          .validate(h => if (h > 0) success else failure("Import height must be > 0")),
        opt[String]('f', "format")
          .hidden()
          .text("Blockchain data file format")
          .action((f, c) => c.copy(format = f))
          .valueName(s"<${Formats.list.mkString("|")}> (default is ${Formats.default})")
          .validate {
            case f if Formats.isSupported(f) => success
            case f                           => failure(s"Unsupported format: $f")
          },
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

  private var triggers = Seq.empty[BlockchainUpdateTriggers]

  def initExtensions(
      wavesSettings: WavesSettings,
      blockchainUpdater: BlockchainUpdaterImpl,
      appenderScheduler: Scheduler,
      extensionTime: Time,
      utxPool: UtxPool,
      rdb: RDB
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
          override def utxEvents: Observable[UtxEvent] = Observable.empty
          override def transactionsApi: CommonTransactionsApi =
            CommonTransactionsApi(
              blockchainUpdater.bestLiquidSnapshot.map(Height(blockchainUpdater.height) -> _),
              rdb,
              blockchainUpdater,
              utxPool,
              None,
              _ => Future.successful(TracedResult.wrapE(Left(GenericError("Not implemented during import")))),
              Application.loadBlockAt(rdb, blockchainUpdater)
            )
          override def blocksApi: CommonBlocksApi =
            CommonBlocksApi(
              settings.synchronizationSettings.maxRollback,
              blockchainUpdater,
              Application.loadBlockMetaAt(rdb, blockchainUpdater),
              Application.loadBlockInfoAt(rdb, blockchainUpdater)
            )
          override def accountsApi: CommonAccountsApi =
            CommonAccountsApi(() => blockchainUpdater.snapshotBlockchain, rdb, blockchainUpdater)
          override def assetsApi: CommonAssetsApi =
            CommonAssetsApi(() => blockchainUpdater.bestLiquidSnapshot.orEmpty, rdb, blockchainUpdater)
          override def generatorsApi: CommonGeneratorsApi =
            CommonGeneratorsApi(rdb, blockchainUpdater)
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

  // Rides out a server restart or a network blip; a dead mirror still fails within about 11 minutes (10 x (5 s + up to 1 min)).
  private val HttpTimeout    = 1.minute
  private val HttpMaxRetries = 10
  private val HttpRetryDelay = 5.seconds

  // Set by the shutdown hook: stops the import before the state is closed.
  @volatile private var quit = false
  private val lock           = new Object

  private type ImportedBlock = (VanillaBlock, Option[BlockSnapshotResponse])

  // What the reader hands to the appender: a block, the end of the input (None), or the error that stopped it.
  private type ReadResult = Either[Throwable, Option[ImportedBlock]]

  // noinspection UnstableApiUsage
  def startImport(
      blocksInputStream: BufferedInputStream,
      snapshotsInputStream: Option[BufferedInputStream],
      blockchain: Blockchain,
      appendBlock: AppendBlock,
      importOptions: ImportOptions,
      skipBlocks: Boolean,
      appender: Scheduler
  ): Unit = {
    val start   = System.nanoTime()
    var counter = 0

    val startHeight   = blockchain.height
    val blocksToSkip  = if (skipBlocks) startHeight - 1 else 0
    val blocksToApply = importOptions.importHeight - startHeight

    if (blocksToSkip > 0) log.info(s"Skipping $blocksToSkip block(s)")

    sys.addShutdownHook {
      import scala.concurrent.duration.*
      val millis = (System.nanoTime() - start).nanos.toMillis
      log.info(
        s"Imported $counter block(s) from $startHeight to ${startHeight + counter} in ${time.Duration.ofMillis(millis)}"
      )
    }

    // Parsing runs on its own thread so that the appender never waits for the next block.
    val queue = new ArrayBlockingQueue[ReadResult](importOptions.maxQueueSize)
    val reader =
      new Thread(() => readBlocks(blocksInputStream, snapshotsInputStream, blockchain, startHeight, blocksToSkip, queue), "importer-reader")
    reader.setDaemon(true)
    reader.start()

    try {
      var stop = false
      while (!stop && !quit && counter < blocksToApply)
        queue.take() match {
          case Left(error) => throw error
          case Right(None) => stop = true
          case Right(Some((block, snapshot))) =>
            lock.synchronized {
              if (blockchain.lastBlockId.contains(block.header.reference)) {
                ImportProbe.appendBlock(startHeight + counter + 1, block.transactionData.size) {
                  Await.result(appendBlock(block, snapshot).runAsyncLogErr(using appender), Duration.Inf)
                } match {
                  case Left(ve) =>
                    log.error(s"Error appending block ${blockchain.height + 1}: $ve")
                    stop = true
                  case _ =>
                    counter = counter + 1
                }
              } else if (!quit) {
                log.warn(s"Block $block is not a child of the last block ${blockchain.lastBlockId.get}")
              }
            }
        }
    } finally reader.interrupt()
  }

  /** Verifies block VRF proofs on the signature pool ahead of the appender, which then finds the results in the VRF cache of
    * [[CachedVrf]]. A proof needs the hit source of an earlier block (101 blocks back under fair PoS), so many run in parallel. The
    * heights come from the same rules as [[PoSSelector.validateGenerationSignature]]; where the feature state differs, such as a feature
    * approved by a block still in the queue, the appender just misses the cache and verifies the proof itself.
    */
  private final class VrfPrecheck(blockchain: Blockchain) {
    private val hitSources = mutable.Map.empty[Int, Future[ByteStr]]

    private def hitSource(height: Int): Future[ByteStr] =
      hitSources.getOrElse(
        height,
        blockchain.hitSource(height).fold(Future.failed[ByteStr](new NoSuchElementException(s"No hit source at $height")))(Future.successful)
      )

    def add(block: Block, height: Int): Unit = {
      val blockHitSource =
        if (PoSSelector.vrfActivated(blockchain, height)) {
          hitSource(PoSSelector.hitSourceHeight(blockchain, height - 1)).map(message =>
            CachedVrf.verify(block.header.generationSignature, message.arr, block.header.generator, checkWeakPk = false).explicitGet()
          )(using sigverify)
        } else Future.successful(block.header.generationSignature)
      hitSources.update(height, blockHitSource)
      hitSources.remove(height - 101)
    }
  }

  // A block in either export format, with its transaction snapshots when the import has them.
  private def parseRecord(blockBytes: Array[Byte], snapshotsBytes: Option[Array[Byte]], currentTs: Long): ImportedBlock = {
    lazy val parsedProtoBlock = PBBlocks.vanilla(PBBlocks.addChainId(protobuf.block.PBBlock.parseFrom(blockBytes)), unsafe = true)
    val block = (if (1 < blockBytes.head && blockBytes.head < 5 && Longs.fromByteArray(blockBytes.slice(1, 9)) < currentTs)
                   Block.parseBytes(blockBytes).orElse(parsedProtoBlock)
                 else
                   parsedProtoBlock).get
    block -> snapshotsBytes.map(bytes => BlockSnapshotResponse(block.id(), parseSnapshots(bytes, block.transactionData.size)))
  }

  // One snapshot per transaction, each a 4-byte length and that many bytes, filling the record exactly (Exporter.IO.exportBlockTxSnapshots).
  private def parseSnapshots(bytes: Array[Byte], count: Int): Seq[TransactionStateSnapshot] = {
    var offset = 0
    val snapshots = Seq.fill(count) {
      require(offset + Ints.BYTES <= bytes.length, s"Snapshot record of ${bytes.length} bytes ends before snapshot length at $offset")
      val size  = Ints.fromByteArray(bytes.slice(offset, offset + Ints.BYTES))
      val start = offset + Ints.BYTES
      require(size >= 0 && size <= bytes.length - start, s"Snapshot of $size bytes at $start does not fit a record of ${bytes.length} bytes")
      offset = start + size
      TransactionStateSnapshot.parseFrom(bytes.slice(start, offset))
    }
    require(offset == bytes.length, s"Snapshot record of ${bytes.length} bytes has ${bytes.length - offset} bytes after $count snapshots")
    snapshots
  }

  private def readBlocks(
      blocksInputStream: BufferedInputStream,
      snapshotsInputStream: Option[BufferedInputStream],
      blockchain: Blockchain,
      startHeight: Int,
      blocksToSkip: Int,
      queue: ArrayBlockingQueue[ReadResult]
  ): Unit = {
    val lenBlockBytes     = new Array[Byte](Ints.BYTES)
    val lenSnapshotsBytes = if (snapshotsInputStream.isDefined) Some(new Array[Byte](Ints.BYTES)) else None
    val CurrentTS         = System.currentTimeMillis()
    var skipped           = 0
    var enqueued          = 0
    val vrfPrecheck       = new VrfPrecheck(blockchain)

    // Reads the next block and its snapshots, or returns false at the end of the input or on a truncated record.
    def readNext(): Boolean = {
      val blockSizeBytesLength    = ByteStreams.read(blocksInputStream, lenBlockBytes, 0, Ints.BYTES)
      val snapshotSizeBytesLength = (snapshotsInputStream, lenSnapshotsBytes).mapN(ByteStreams.read(_, _, 0, Ints.BYTES))
      if (blockSizeBytesLength == Ints.BYTES && snapshotSizeBytesLength.forall(_ == Ints.BYTES)) {
        val blockSize     = Ints.fromByteArray(lenBlockBytes)
        val snapshotsSize = lenSnapshotsBytes.map(Ints.fromByteArray)

        lazy val blockBytes     = new Array[Byte](blockSize)
        lazy val snapshotsBytes = snapshotsSize.map(new Array[Byte](_))
        val (factReadBlockSize, factReadSnapshotsSize) =
          if (skipped < blocksToSkip) {
            // File IO optimization
            ByteStreams.skipFully(blocksInputStream, blockSize)
            (snapshotsInputStream, snapshotsSize).mapN(ByteStreams.skipFully(_, _))

            blockSize -> snapshotsSize
          } else {
            (
              ByteStreams.read(blocksInputStream, blockBytes, 0, blockSize),
              (snapshotsInputStream, snapshotsBytes, snapshotsSize).mapN(ByteStreams.read(_, _, 0, _))
            )
          }

        if (factReadBlockSize == blockSize && factReadSnapshotsSize == snapshotsSize) {
          if (skipped < blocksToSkip) {
            skipped += 1
          } else {
            val height                 = startHeight + enqueued + 1
            val rideV6                 = blockchain.isFeatureActivated(BlockchainFeatures.RideV6, height)
            val (block, blockSnapshot) = parseRecord(blockBytes, snapshotsBytes, CurrentTS)

            ParSignatureChecker.checkBlockAndTxSignatures(block, blockSnapshot.isEmpty, rideV6)
            vrfPrecheck.add(block, height)

            queue.put(Right(Some(block -> blockSnapshot)))
            enqueued += 1
          }
          true
        } else {
          if (factReadBlockSize != blockSize)
            log.info(s"$factReadBlockSize != expected $blockSize for blocks")
          if (factReadSnapshotsSize != snapshotsSize)
            log.info(s"$factReadSnapshotsSize != expected $snapshotsSize for snapshots")
          false
        }
      } else {
        if (blocksInputStream.available() > 0 && blockSizeBytesLength != Ints.BYTES)
          log.info(s"Expecting to read ${Ints.BYTES} but got $blockSizeBytesLength (${blocksInputStream.available()})")
        (snapshotsInputStream, snapshotSizeBytesLength) match {
          case (Some(is), Some(sizeBytesLength)) if is.available() > 0 && sizeBytesLength != Ints.BYTES =>
            log.info(s"Expecting to read ${Ints.BYTES} but got $sizeBytesLength (${is.available()})")
          case _ => ()
        }
        false
      }
    }

    try {
      while (readNext()) ()
      queue.put(Right(None))
    } catch {
      case _: InterruptedException => () // the import ended before the input did
      case e: Throwable            =>
        // The appender rethrows it. Fatal errors too: the appender would otherwise wait for the next block forever.
        try queue.put(Left(e))
        catch { case _: InterruptedException => () }
    }
  }

  def main(args: Array[String]): Unit = {
    val importOptions = parseOptions(args)
    val settings      = loadSettings(importOptions.configFile)
    log.info(s"Importer ${Version.VersionString}")
    log.info(s"RIDE evaluator: ${ScopeEvaluator.mode}")
    // A mismatch thrown inside an invoke can end up as a failed transaction, and the import would go on with a different state.
    if (ScopeEvaluator.mode == ScopeEvaluator.Mode.Check) ScopeEvaluator.onMismatch = { message =>
      log.error(s"Evaluator mismatch: ${message.take(20000)}")
      Runtime.getRuntime.halt(3)
    }
    Application.withKamon(settings.config) {
      def initFileStream(file: String, offset: Long): InputStream = {
        log.info(s"Opening import file: $file, offset=$offset")
        file match {
          case "-" =>
            System.in

          case _ =>
            System.setProperty("http.agent", s"waves-node/${Version.VersionString}")
            val uri    = new URI(file)
            val scheme = Option(uri.getScheme).map(_.toLowerCase(Locale.ROOT))
            if (scheme.exists(Set("http", "https"))) {
              new ResumableInputStream(ResumableInputStream.openHttp(uri.toURL, HttpTimeout), offset, HttpMaxRetries, HttpRetryDelay)
            } else if (scheme.exists(_ != "file")) {
              // Other URL schemes have no Range request, so read up to the offset instead.
              val in = uri.toURL.openStream()
              ByteStreams.skipFully(in, offset)
              in
            } else {
              val fs = new FileInputStream(file)
              if (offset > 0) fs.skip(offset)
              fs
            }
        }
      }

      val scheduler = Schedulers.singleThread("appender")
      val time      = Time(settings.ntpServer)

      val rdb = RDB.open(settings.dbSettings)
      val (blockchainUpdater, rdbWriter) =
        StorageFactory(settings, rdb, time, BlockchainUpdateTriggers.combined(triggers))
      ImportProbe.watch(rdb, () => rdbWriter.cacheStats)
      val utxPool = new UtxPoolImpl(time, blockchainUpdater, settings.utxSettings, settings.maxTxErrorLogSize, settings.minerSettings.enable)
      val pos     = PoSSelector(blockchainUpdater, settings.synchronizationSettings.maxBaseTarget)
      val extAppender: AppendBlock = {
        val append =
          BlockAppender(blockchainUpdater, time, utxPool, pos, BlockEndorser.Disabled, scheduler, importOptions.verify, txSignParCheck = false)
        (block, snapshot) =>
          append(block, snapshot).map { result =>
            // The appended block is the liquid one, and its snapshot sums the complexity of its transactions.
            if (result.isRight) ImportProbe.blockApplied(blockchainUpdater.bestLiquidSnapshot.fold(0L)(_.scriptsComplexity))
            result
          }
      }

      val extensions = initExtensions(settings, blockchainUpdater, scheduler, time, utxPool, rdb)
      checkGenesis(settings, blockchainUpdater, Miner.StrictDisabledMiner)

      val blocksFileOffset = importOptions.format match {
        case Formats.Binary =>
          var blocksOffset = 0L
          rdb.db.iterateBlockMeta(rdb.blockMetaHandle) { (height, meta) =>
            if (height.toInt > 1) blocksOffset += meta.size + 4 // Skip genesis
          }
          blocksOffset

        case _ => 0
      }

      val blocksInputStream = new BufferedInputStream(initFileStream(importOptions.blockchainFile, blocksFileOffset), 2 * 1024 * 1024)
      val snapshotsInputStream = importOptions.snapshotsFile.map { file =>
        val inputStream = new BufferedInputStream(initFileStream(file, 0), 20 * 1024 * 1024)
        val sizeBytes   = new Array[Byte](Ints.BYTES)
        (2 to blockchainUpdater.height).foreach { _ =>
          ByteStreams.read(inputStream, sizeBytes, 0, 4)
          val snapshotsSize = Ints.fromByteArray(sizeBytes)
          ByteStreams.skipFully(inputStream, snapshotsSize)
        }
        inputStream
      }

      sys.addShutdownHook {
        quit = true
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
                featureVotes = Nil,
                rewardVote = 0,
                transactionsRoot = ByteStr.empty,
                stateHash = None,
                challengedHeader = None,
                finalizationVoting = None
              ),
              ByteStr.empty,
              Nil
            )
            // On the appender, like every other write: BlockchainUpdaterImpl validates outside its write lock, so writers must not overlap
            Await.result(
              Task(blockchainUpdater.processBlock(pseudoBlock, hitSource = ByteStr.empty, snapshot = None, generatorSet = Seq.empty, verify = false))
                .runAsyncLogErr(using scheduler),
              Duration.Inf
            )
          }

          // Terminate appender
          scheduler.shutdown()
          scheduler.awaitTermination(10 seconds)

          // Terminate extensions
          Await.ready(Future.sequence(extensions.map(_.shutdown())), settings.extensionsShutdownTimeout)

          utxPool.close()
          blockchainUpdater.shutdown()
          rdbWriter.close()
          ImportProbe.unwatch()
          rdb.close()
        }
        blocksInputStream.close()
        snapshotsInputStream.foreach(_.close())
      }

      startImport(
        blocksInputStream,
        snapshotsInputStream,
        blockchainUpdater,
        extAppender,
        importOptions,
        blocksFileOffset == 0,
        scheduler
      )
    }
  }
}
