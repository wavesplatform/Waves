package com.wavesplatform

import akka.actor.ActorSystem
import cats.implicits.catsSyntaxOption
import cats.syntax.apply.*
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
import com.wavesplatform.mining.Miner
import com.wavesplatform.network.BlockSnapshotResponse
import com.wavesplatform.protobuf.block.{PBBlocks, VanillaBlock}
import com.wavesplatform.protobuf.snapshot.TransactionStateSnapshot
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import com.wavesplatform.state.ParSignatureChecker.sigverify
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.{Blockchain, BlockchainUpdaterImpl, Height, ParSignatureChecker}
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
import java.time
import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration.*
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

object Importer extends ScorexLogging {

  type AppendBlock = (Block, Option[BlockSnapshotResponse]) => Task[Either[ValidationError, BlockApplyResult]]

  final case class ImportOptions(
      configFile: Option[File] = None,
      blockchainFile: String = "blockchain",
      snapshotsFile: String = "snapshots",
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
          .action((f, c) => c.copy(snapshotsFile = f)),
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
              None,
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
      blocksInputStream: BufferedInputStream,
      snapshotsInputStream: Option[BufferedInputStream],
      blockchain: Blockchain,
      appendBlock: AppendBlock,
      importOptions: ImportOptions,
      skipBlocks: Boolean,
      appender: Scheduler
  ): Unit = {
    val lenBlockBytes     = new Array[Byte](Ints.BYTES)
    val lenSnapshotsBytes = if (snapshotsInputStream.isDefined) Some(new Array[Byte](Ints.BYTES)) else None
    val start             = System.nanoTime()
    var counter           = 0

    val startHeight   = blockchain.height
    var blocksToSkip  = if (skipBlocks) startHeight - 1 else 0
    val blocksToApply = importOptions.importHeight - startHeight + 1

    if (blocksToSkip > 0) log.info(s"Skipping $blocksToSkip block(s)")

    sys.addShutdownHook {
      import scala.concurrent.duration.*
      val millis = (System.nanoTime() - start).nanos.toMillis
      log.info(
        s"Imported $counter block(s) from $startHeight to ${startHeight + counter} in ${time.Duration.ofMillis(millis)}"
      )
    }

    val maxSize = importOptions.maxQueueSize
    val queue   = new mutable.Queue[(VanillaBlock, Option[BlockSnapshotResponse])](maxSize)

    @tailrec
    def readBlocks(queue: mutable.Queue[(VanillaBlock, Option[BlockSnapshotResponse])], remainCount: Int, maxCount: Int): Unit = {
      if (remainCount == 0) ()
      else {
        val blockSizeBytesLength    = ByteStreams.read(blocksInputStream, lenBlockBytes, 0, Ints.BYTES)
        val snapshotSizeBytesLength = (snapshotsInputStream, lenSnapshotsBytes).mapN(ByteStreams.read(_, _, 0, Ints.BYTES))
        if (blockSizeBytesLength == Ints.BYTES && snapshotSizeBytesLength.forall(_ == Ints.BYTES)) {
          val blockSize     = Ints.fromByteArray(lenBlockBytes)
          val snapshotsSize = lenSnapshotsBytes.map(Ints.fromByteArray)

          lazy val blockBytes     = new Array[Byte](blockSize)
          lazy val snapshotsBytes = snapshotsSize.map(new Array[Byte](_))
          val (factReadBlockSize, factReadSnapshotsSize) =
            if (blocksToSkip > 0) {
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
            if (blocksToSkip > 0) {
              blocksToSkip -= 1
            } else {
              val blockV5               = blockchain.isFeatureActivated(BlockchainFeatures.BlockV5, blockchain.height + (maxCount - remainCount) + 1)
              val rideV6                = blockchain.isFeatureActivated(BlockchainFeatures.RideV6, blockchain.height + (maxCount - remainCount) + 1)
              lazy val parsedProtoBlock = PBBlocks.vanilla(PBBlocks.addChainId(protobuf.block.PBBlock.parseFrom(blockBytes)), unsafe = true)

              val block = (if (!blockV5) Block.parseBytes(blockBytes) else parsedProtoBlock).orElse(parsedProtoBlock).get
              val blockSnapshot = snapshotsBytes.map { bytes =>
                BlockSnapshotResponse(
                  block.id(),
                  block.transactionData
                    .foldLeft((0, Seq.empty[TransactionStateSnapshot])) { case ((offset, acc), _) =>
                      val txSnapshotSize = Ints.fromByteArray(bytes.slice(offset, offset + Ints.BYTES))
                      val txSnapshot     = TransactionStateSnapshot.parseFrom(bytes.slice(offset + Ints.BYTES, offset + Ints.BYTES + txSnapshotSize))
                      (offset + Ints.BYTES + txSnapshotSize, txSnapshot +: acc)
                    }
                    ._2
                    .reverse
                )
              }

              ParSignatureChecker.checkBlockAndTxSignatures(block, blockSnapshot.isEmpty, rideV6)

              queue.enqueue(block -> blockSnapshot)
            }
            readBlocks(queue, remainCount - 1, maxCount)
          } else {
            if (factReadBlockSize != blockSize)
              log.info(s"$factReadBlockSize != expected $blockSize for blocks")
            if (factReadSnapshotsSize == snapshotsSize)
              log.info(s"$factReadSnapshotsSize != expected $snapshotsSize for snapshots")

            quit = true
          }
        } else {
          if (blocksInputStream.available() > 0 && blockSizeBytesLength != Ints.BYTES)
            log.info(s"Expecting to read ${Ints.BYTES} but got $blockSizeBytesLength (${blocksInputStream.available()})")
          (snapshotsInputStream, snapshotSizeBytesLength) match {
            case (Some(is), Some(sizeBytesLength)) if is.available() > 0 && sizeBytesLength != Ints.BYTES =>
              log.info(s"Expecting to read ${Ints.BYTES} but got $sizeBytesLength (${is.available()})")
            case _ => ()
          }

          quit = true
        }
      }
    }

    while ((!quit || queue.nonEmpty) && counter < blocksToApply)
      if (!quit && queue.isEmpty) {
        readBlocks(queue, maxSize, maxSize)
      } else {
        lock.synchronized {
          val (block, snapshot) = queue.dequeue()
          if (blockchain.lastBlockId.contains(block.header.reference)) {
            Await.result(appendBlock(block, snapshot).runAsyncLogErr(appender), Duration.Inf) match {
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
    val utxPool = new UtxPoolImpl(time, blockchainUpdater, settings.utxSettings, settings.maxTxErrorLogSize, settings.minerSettings.enable)
    val pos     = PoSSelector(blockchainUpdater, settings.synchronizationSettings.maxBaseTarget)
    val extAppender: (Block, Option[BlockSnapshotResponse]) => Task[Either[ValidationError, BlockApplyResult]] =
      BlockAppender(blockchainUpdater, time, utxPool, pos, scheduler, importOptions.verify, txSignParCheck = false)

    val extensions = initExtensions(settings, blockchainUpdater, scheduler, time, utxPool, rdb, actorSystem)
    checkGenesis(settings, blockchainUpdater, Miner.Disabled)

    val (blocksFileOffset, snapshotsFileOffset) =
      importOptions.format match {
        case Formats.Binary =>
          var blocksOffset = 0L
          rdb.db.iterateOver(KeyTags.BlockInfoAtHeight) { e =>
            e.getKey match {
              case Array(_, _, 0, 0, 0, 1) => // Skip genesis
              case _ =>
                val meta = com.wavesplatform.database.readBlockMeta(e.getValue)
                blocksOffset += meta.size + 4
            }
          }
          val snapshotsOffset = (2 to blockchainUpdater.height).map { h =>
            database.loadTxStateSnapshots(Height(h), rdb).map(_.toByteArray.length).sum
          }.sum

          blocksOffset -> snapshotsOffset.toLong
        case _ => 0L -> 0L
      }
    val blocksInputStream = new BufferedInputStream(initFileStream(importOptions.blockchainFile, blocksFileOffset), 2 * 1024 * 1024)
    val snapshotsInputStream =
      if (settings.enableLightMode)
        Some(new BufferedInputStream(initFileStream(importOptions.snapshotsFile, snapshotsFileOffset), 20 * 1024 * 1024))
      else None

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
          blockchainUpdater.processBlock(pseudoBlock, ByteStr.empty, None, verify = false)
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
    Await.result(Kamon.stopModules(), 10.seconds)
  }
}
