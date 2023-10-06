package com.wavesplatform.utils.generator

import java.io.{File, FileOutputStream, PrintWriter}
import java.util.concurrent.TimeUnit

import cats.implicits.*
import com.typesafe.config.{ConfigFactory, ConfigParseOptions}
import com.wavesplatform.{GenesisBlockGenerator, Version}
import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.database.RDB
import com.wavesplatform.events.{BlockchainUpdateTriggers, UtxEvent}
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.{Miner, MinerImpl}
import com.wavesplatform.settings.*
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.{Schedulers, ScorexLogging, Time}
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import monix.reactive.subjects.ConcurrentSubject
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*
import play.api.libs.json.Json
import scopt.OParser

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.*
import scala.language.reflectiveCalls

object BlockchainGeneratorApp extends ScorexLogging {
  final case class BlockchainGeneratorAppSettings(
      genesisConfigFile: File = null,
      configFile: Option[File] = None,
      outputFile: Option[File] = None,
      blocks: Int = 1000,
      targetAverageTime: Option[Int] = None,
      miningConflictInterval: Option[Int] = None
  )

  def parseOptions(args: Array[String]): BlockchainGeneratorAppSettings = {
    lazy val commandParser = {
      import scopt.OParser

      val builder = OParser.builder[BlockchainGeneratorAppSettings]
      import builder.*

      OParser.sequence(
        programName("waves blockchain generator"),
        head("Waves Blockchain Generator", Version.VersionString),
        opt[File]("genesis-config")
          .required()
          .abbr("gc")
          .text("Genesis config file name")
          .action((f, c) => c.copy(genesisConfigFile = f)),
        opt[File]('c', "config")
          .text("Config file name")
          .action((f, c) => c.copy(configFile = Some(f))),
        opt[Int]('t', "target")
          .text("Target average time")
          .action((f, c) => c.copy(targetAverageTime = Some(f))),
        opt[File]('o', "output")
          .text("Output file name")
          .action((f, c) => c.copy(outputFile = Some(f))),
        opt[Int]('b', "blocks")
          .text("Blocks count")
          .action((h, c) => c.copy(blocks = h))
          .validate(h => if (h > 0) success else failure("Blocks must be > 0")),
        opt[Int]("mining-conflict")
          .abbr("mc")
          .text("Mining conflict interval (in milliseconds)")
          .action((mc, c) => c.copy(miningConflictInterval = Some(mc))),
        help("help").hidden()
      )
    }

    OParser
      .parse(commandParser, args, BlockchainGeneratorAppSettings())
      .getOrElse {
        println(OParser.usage(commandParser))
        sys.exit(1)
      }
  }

  def start(options: BlockchainGeneratorAppSettings): Unit = {
    implicit val scheduler = Schedulers.singleThread("blockchain-generator")
    sys.addShutdownHook(synchronized {
      scheduler.shutdown()
      scheduler.awaitTermination(10 seconds)
    })

    def readConfFile(f: File) = ConfigFactory.parseFile(f, ConfigParseOptions.defaults().setAllowMissing(false))

    val config      = readConfFile(options.genesisConfigFile)
    val genSettings = GenesisBlockGenerator.parseSettings(config)
    val genesis     = ConfigFactory.parseString(GenesisBlockGenerator.createConfig(genSettings)).as[GenesisSettings]("genesis")

    log.info(s"Initial base target is ${genesis.initialBaseTarget}")

    val blockchainSettings = BlockchainSettings(genSettings.chainId.toChar, genSettings.functionalitySettings, genesis, RewardsSettings.MAINNET)
    val wavesSettings = {
      val settings = WavesSettings.fromRootConfig(loadConfig(options.configFile.map(readConfFile)))
      settings.copy(blockchainSettings = blockchainSettings, minerSettings = settings.minerSettings.copy(quorum = 0))
    }

    val fakeTime = new Time {
      val startTime: Long = genSettings.timestamp.getOrElse(System.currentTimeMillis())

      @volatile
      var time: Long = startTime

      override def correctedTime(): Long = time
      override def getTimestamp(): Long  = time
    }

    val blockchain = {
      val rdb = RDB.open(wavesSettings.dbSettings)
      val (blockchainUpdater, rocksdb) =
        StorageFactory(wavesSettings, rdb, fakeTime, BlockchainUpdateTriggers.noop)
      com.wavesplatform.checkGenesis(wavesSettings, blockchainUpdater, Miner.Disabled)
      sys.addShutdownHook(synchronized {
        blockchainUpdater.shutdown()
        rdb.close()
      })
      blockchainUpdater
    }

    val miners = genSettings.distributions.collect {
      case item if item.miner =>
        val info = GenesisBlockGenerator.toFullAddressInfo(item)
        info.account
    }

    val wallet: Wallet = new Wallet {
      private[this] val map                                            = miners.map(kp => kp.toAddress -> kp).toMap
      override def seed: Array[Byte]                                   = Array.emptyByteArray
      override def nonce: Int                                          = miners.length
      override def privateKeyAccounts: Seq[SeedKeyPair]                = miners
      override def generateNewAccounts(howMany: Int): Seq[SeedKeyPair] = ???
      override def generateNewAccount(): Option[SeedKeyPair]           = ???
      override def generateNewAccount(nonce: Int): Option[SeedKeyPair] = ???
      override def deleteAccount(account: SeedKeyPair): Boolean        = ???
      override def privateKeyAccount(account: Address): Either[ValidationError, SeedKeyPair] =
        map.get(account).toRight(GenericError(s"No key for $account"))
    }

    val utx = new UtxPoolImpl(fakeTime, blockchain, wavesSettings.utxSettings, wavesSettings.maxTxErrorLogSize, wavesSettings.minerSettings.enable)
    val posSelector = PoSSelector(blockchain, None)
    val utxEvents   = ConcurrentSubject.publish[UtxEvent](scheduler)
    val miner = new MinerImpl(
      new DefaultChannelGroup("", null),
      blockchain,
      wavesSettings,
      fakeTime,
      utx,
      wallet,
      posSelector,
      scheduler,
      scheduler,
      utxEvents.collect { case _: UtxEvent.TxAdded => () }
    )
    val blockAppender = BlockAppender(blockchain, fakeTime, utx, posSelector, scheduler, verify = false)(_, None)

    object Output {
      private[this] var first = true
      private[this] val output = options.outputFile.map { f =>
        log.info(s"Blocks json will be written to $f")
        val fs = new FileOutputStream(f)
        new PrintWriter(fs)
      }

      synchronized {
        output.foreach(_.print("["))
        sys.addShutdownHook(Output.finish())
      }

      def writeBlock(block: Block): Unit =
        synchronized(output.foreach { output =>
          if (!first) output.print(",")
          first = false
          val json = Json.prettyPrint(block.json())
          json.linesIterator.foreach(line => output.print(System.lineSeparator() + "    " + line))
        })

      def finish(): Unit =
        synchronized(output.foreach { output =>
          output.println(System.lineSeparator() + "]")
          output.close()
        })
    }

    val blocks = ArrayBuffer.empty[Block]

    def averageTime(): FiniteDuration = {
      val (_, delaySum) = blocks.foldLeft(fakeTime.startTime -> 0L) { case ((prevTs, delays), block) =>
        val ts = block.header.timestamp
        (ts, ts - prevTs + delays)
      }
      delaySum.millis / blocks.length.max(1)
    }

    def checkAverageTime(): Boolean = {
      val avgSeconds = averageTime().toUnit(TimeUnit.SECONDS)
      log.info(f"Average block time is $avgSeconds%.2f seconds")

      options.targetAverageTime match {
        case Some(target) => math.abs(avgSeconds - target) < 0.2
        case None         => true
      }
    }

    var conflictCounter = 0

    var quit = false
    sys.addShutdownHook {
      log.info(s"Found $conflictCounter miner conflicts")
      log.info(f"Average block time is ${averageTime().toUnit(TimeUnit.SECONDS)}%.2f seconds")
      quit = true
    }

    while (!Thread.currentThread().isInterrupted && !quit) synchronized {
      val times = miners.flatMap { kp =>
        val time = miner.nextBlockGenerationTime(blockchain, blockchain.height, blockchain.lastBlockHeader.get, kp)
        time.toOption.map(kp -> _)
      }

      for {
        mcInterval <- options.miningConflictInterval
        sorted = times.map(_._2).sorted
        firstMiningTime  <- sorted.headOption
        secondMiningTime <- sorted.drop(1).headOption
      } yield {
        if (secondMiningTime - firstMiningTime < mcInterval) {
          conflictCounter += 1
          log.warn(s"Mining conflict: $firstMiningTime and $secondMiningTime")
        }
      }

      val (bestMiner, nextTime) = times.minBy(_._2)
      fakeTime.time = nextTime

      miner.forgeBlock(bestMiner) match {
        case Right((block, _)) =>
          blockAppender(block).runSyncUnsafe() match {
            case Right(_) =>
              blocks += block
              Output.writeBlock(block)
              if (checkAverageTime() && blockchain.height > options.blocks) sys.exit(0)

            case Left(err) =>
              log.error(s"Error appending block: $err")
              sys.exit(1)
          }

        case Left(err) =>
          log.error(s"Error generating block: $err")
          sys.exit(1)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val options = parseOptions(args)
    start(options)
  }
}
