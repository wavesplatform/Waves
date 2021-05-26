package com.wavesplatform

import java.io._
import java.net.{MalformedURLException, URL}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

import akka.actor.ActorSystem
import com.wavesplatform.account.Address
import com.wavesplatform.api.common.{CommonAccountsApi, CommonAssetsApi, CommonBlocksApi, CommonTransactionsApi}
import com.wavesplatform.block.{Block, BlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.database.{openDB, DBExt, KeyTags}
import com.wavesplatform.events.{BlockchainUpdateTriggers, UtxEvent}
import com.wavesplatform.extensions.{Context, Extension}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.Miner
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.{Blockchain, BlockchainUpdaterImpl, Diff, Height}
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.transaction.{Asset, DiscardedBlocks, Transaction}
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utils._
import com.wavesplatform.utx.{UtxPool, UtxPoolImpl}
import com.wavesplatform.wallet.Wallet
import kamon.Kamon
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.{Observable, Observer}
import org.iq80.leveldb.DB
import scopt.OParser

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success, Try}

class BackoffPolicy(initialBackoff: Duration, maxBackoff: Duration) {
  private final val Multiplier = 1.6
  private final val Jitter     = .2

  private var nextBackoffNanos = initialBackoff.toMillis

  def nextBackoff(): Long = {
    val currentBackoff = nextBackoffNanos
    nextBackoffNanos = (nextBackoffNanos * Multiplier).toLong.min(maxBackoff.toMillis)
    currentBackoff + jitter(currentBackoff)
  }

  private def jitter(baseValue: Long): Long = {
    val jitterValue = baseValue * Jitter
    ((-jitterValue * .5) + Random.nextDouble() * jitterValue).toLong
  }

  def reset(): Unit = {
    nextBackoffNanos = initialBackoff.toMillis
  }
}

object Importer extends ScorexLogging {
  import monix.execution.Scheduler.Implicits.global

  type AppendBlock = Block => Task[Either[ValidationError, Option[BigInt]]]

  final case class ImportOptions(
      configFile: Option[File] = None,
      blockchainFile: String = "blockchain",
      importHeight: Int = Int.MaxValue,
      verify: Boolean = true,
      dryRun: Boolean = false
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
          .action((f, c) => c.copy(configFile = Some(f))),
        opt[String]('i', "input-file")
          .required()
          .text("Blockchain data file name")
          .action((f, c) => c.copy(blockchainFile = f)),
        opt[Int]('h', "height")
          .text("Import to height")
          .action((h, c) => c.copy(importHeight = h))
          .validate(h => if (h > 0) success else failure("Import height must be > 0")),
        opt[Unit]('n', "no-verify")
          .text("Disable signatures verification")
          .action((_, c) => c.copy(verify = false)),
        opt[Unit]("dry-run").action((_, c) => c.copy(dryRun = true)),
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
              wallet,
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

  private val backoffPolicy = new BackoffPolicy(1.second, 2.minutes)

  @tailrec
  def withBackoff(f: => Unit): Unit =
    try f
    catch {
      case e: IOException =>
        val delay = backoffPolicy.nextBackoff()
        log.info(f"Next attempt in ${delay * 1e-3}%.3f seconds", e)
        Thread.sleep(delay)
        withBackoff(f)
    }

  def importStep(data: DataInputStream, appendBlock: AppendBlock, blockchain: Blockchain, targetHeight: Int): Unit =
    while (!quit && blockchain.height < targetHeight) lock.synchronized {
      data.readInt() // block size is ignored
      val block =
        (if (blockchain.isFeatureActivated(BlockchainFeatures.BlockV5, blockchain.height + 1))
           PBBlocks.vanilla(PBBlocks.addChainId(protobuf.block.PBBlock.parseFrom(data)), unsafe = true)
         else Block.parseBytes(data)) match {
          case Success(value) =>
            if (value.signatureValid()) {
              backoffPolicy.reset()
              value
            } else
              throw new IOException("Error parsing block: invalid signature")

          case Failure(exception) =>
            throw new IOException("Error parsing block", exception)
        }

      Await.result(appendBlock(block).runAsyncLogErr, Duration.Inf) match {
        case Left(error) => throw new IllegalArgumentException(error.toString)
        case Right(_) =>
          if (targetHeight == blockchain.height) quit = true
      }
    }

  def readOffsetFromDB(db: DB): Long = {
    var result = 0L
    db.iterateOver(KeyTags.BlockInfoAtHeight) { e =>
      e.getKey match {
        case Array(_, _, 0, 0, 0, 1) => // Skip genesis
        case _ =>
          result += com.wavesplatform.database.readBlockMeta(e.getValue).size + 4
      }
    }
    result
  }

  def initInputStream(file: String, offset: Long): InputStream =
    file match {
      case "-" =>
        System.in

      case _ =>
        try {
          val url        = new URL(file)
          val connection = url.openConnection()
          connection.setReadTimeout(1000)
          connection.setRequestProperty("user-agent", s"waves-node/${Version.VersionString}")
          if (offset > 0) connection.setRequestProperty("Range", s"bytes=$offset-")
          log.info(s"Offset: $offset")
          connection.connect()
          connection.getInputStream
        } catch {
          case _: MalformedURLException =>
            val fs = new FileInputStream(file)
            if (offset > 0) fs.skip(offset)
            fs
        }
    }

  def main(args: Array[String]): Unit = {
    val importOptions = parseOptions(args)
    val settings      = loadSettings(importOptions.configFile)
    val scheduler     = Schedulers.singleThread("appender")
    val time          = new NTP(settings.ntpServer)
    val actorSystem   = ActorSystem("wavesplatform-import")
    val db            = openDB(settings.dbSettings.directory)
    val (blockchainUpdater, levelDb) =
      StorageFactory(settings, db, time, Observer.empty, BlockchainUpdateTriggers.combined(triggers))
    val utxPool     = new UtxPoolImpl(time, blockchainUpdater, settings.utxSettings)
    val pos         = PoSSelector(blockchainUpdater, settings.synchronizationSettings.maxBaseTarget)
    val extAppender = BlockAppender(blockchainUpdater, time, utxPool, pos, scheduler, importOptions.verify) _

    val extensions = initExtensions(settings, blockchainUpdater, scheduler, time, utxPool, db, actorSystem)
    checkGenesis(settings, blockchainUpdater, Miner.Disabled)
    val initialHeight = blockchainUpdater.height
    val startupTime   = System.currentTimeMillis()

    if (importOptions.dryRun) {
      def readNextBlock(): Future[Option[Block]] = Future.successful(None)
      readNextBlock().flatMap {
        case None => Future.successful(())
        case Some(block) =>

          readNextBlock()
      }
    }

    sys.addShutdownHook {
      quit = true
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

        log.info(s"Imported ${blockchainUpdater.height - initialHeight} blocks in ${humanReadableDuration(System.currentTimeMillis() - startupTime)}")

        // Terminate extensions
        Await.ready(Future.sequence(extensions.map(_.shutdown())), settings.extensionsShutdownTimeout)

        blockchainUpdater.shutdown()
        levelDb.close()
        db.close()
        Await.result(Kamon.stopModules(), 10.seconds)
      }
    }

    withBackoff {
      importStep(
        new DataInputStream(new BufferedInputStream(initInputStream(importOptions.blockchainFile, readOffsetFromDB(db)), 2 * 1024 * 1024)),
        extAppender,
        blockchainUpdater,
        importOptions.importHeight
      )
    }

  }
}
