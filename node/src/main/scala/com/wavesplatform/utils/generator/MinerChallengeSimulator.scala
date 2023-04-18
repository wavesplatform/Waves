package com.wavesplatform.utils.generator

import com.typesafe.config.{ConfigFactory, ConfigParseOptions}
import com.wavesplatform.GenesisBlockGenerator
import com.wavesplatform.account.{Address, SeedKeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.block.Block.BlockId
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.database.{RDB, RocksDBWriter, loadActiveLeases}
import com.wavesplatform.events.{BlockchainUpdateTriggers, UtxEvent}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.StorageFactory
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.{Miner, MinerImpl}
import com.wavesplatform.settings.*
import com.wavesplatform.state.{BalanceSnapshot, BlockchainUpdaterImpl}
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.utils.{Schedulers, ScorexLogging, Time}
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import monix.eval.Task
import monix.reactive.subjects.ConcurrentSubject
import net.ceedubs.ficus.Ficus.*
import net.ceedubs.ficus.readers.ArbitraryTypeReader.*
import org.apache.commons.io.FileUtils

import java.io.File
import scala.concurrent.duration.*
import scala.language.reflectiveCalls

object MinerChallengeSimulator extends App with ScorexLogging {
  implicit val scheduler = Schedulers.singleThread("blockchain-generator")
  sys.addShutdownHook(synchronized {
    scheduler.shutdown()
    scheduler.awaitTermination(10 seconds)
  })

  def readConfFile(f: File) = ConfigFactory.parseFile(f, ConfigParseOptions.defaults().setAllowMissing(false))

  val config      = readConfFile(new File("genesis.conf"))
  val genSettings = GenesisBlockGenerator.parseSettings(config)
  val genesis     = ConfigFactory.parseString(GenesisBlockGenerator.createConfig(genSettings)).as[GenesisSettings]("genesis")

  val blockchainSettings = BlockchainSettings(
    genSettings.chainId.toChar,
    genSettings.functionalitySettings.copy(preActivatedFeatures = BlockchainFeatures.implemented.map(_ -> 0).toMap),
    genesis,
    RewardsSettings.MAINNET
  )
  val wavesSettings = {
    val settings = WavesSettings.fromRootConfig(loadConfig(Some(new File("config.conf")).map(readConfFile)))
    settings.copy(blockchainSettings = blockchainSettings, minerSettings = settings.minerSettings.copy(quorum = 0))
  }

  val forkHeight = 500000

  val miners = genSettings.distributions.collect {
    case item if item.miner =>
      val info = GenesisBlockGenerator.toFullAddressInfo(item)
      info.account
  }

  val maliciousMiner   = miners.head
  val challengingMiner = miners(1)

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

  var originalBlockchain                               = BlockchainObjects.createOriginal()
  var challengingBlockchain: Option[BlockchainObjects] = None

  var quit = false
  sys.addShutdownHook {
    quit = true
  }

  while (!Thread.currentThread().isInterrupted && !quit) synchronized {
    val prevTime         = originalBlockchain.fakeTime
    val originalScore    = originalBlockchain.forgeAndAppendBlock().get
    val challengingScore = challengingBlockchain.flatMap(_.forgeAndAppendBlock())

    if (originalBlockchain.blockchain.height > forkHeight - 1) {
      if (challengingScore.exists(_ <= originalScore)) {
        println(s"Original score = $originalScore, challenging score = ${challengingScore.get} on height ${originalBlockchain.blockchain.height}")
      } else if (challengingScore.isDefined) {
        println(s"Original score ($originalScore) < challenging score (${challengingScore.get}) on height ${originalBlockchain.blockchain.height}")
        quit = true
      }
    }

    if (originalBlockchain.blockchain.height == forkHeight + 1 && challengingBlockchain.isEmpty) {
      originalBlockchain.blockchain.shutdown()
      originalBlockchain.rdb.close()
      challengingBlockchain = Some(BlockchainObjects.createChallenging())
      originalBlockchain = BlockchainObjects.createOriginal(prevTime.time)
    }
  }

  case class BlockchainObjects(
      blockchain: BlockchainUpdaterImpl,
      rdb: RDB,
      miner: MinerImpl,
      blockAppender: Block => Task[Either[ValidationError, Option[BigInt]]],
      fakeTime: Time & Object { var time: Long }
  ) {
    def forgeAndAppendBlock(): Option[BigInt] = {
      val (bestMiner, nextTime) = getBlockMiningTimes.minBy(_._2)
      fakeTime.time = nextTime

      miner.forgeBlock(bestMiner) match {
        case Right((block, _)) =>
          blockAppender(block).runSyncUnsafe() match {
            case Right(score) => score
            case Left(err) =>
              log.error(s"Error appending block: $err")
              quit = true
              Some(0)
          }

        case Left(err) =>
          log.error(s"Error generating block: $err")
          quit = true
          Some(0)
      }
    }

    private def getBlockMiningTimes: Seq[(SeedKeyPair, Long)] =
      miners.flatMap { kp =>
        val time = miner.nextBlockGenerationTime(blockchain, blockchain.height, blockchain.lastBlockHeader.get, kp)
        time.toOption.map(kp -> _)
      }
  }

  object BlockchainObjects {
    def createOriginal(startTime: Long = genSettings.timestamp.getOrElse(System.currentTimeMillis())): BlockchainObjects = {
      val rdb      = RDB.open(wavesSettings.dbSettings)
      val fakeTime = createFakeTime(startTime)
      val (blockchainUpdater, _) =
        StorageFactory(wavesSettings, rdb, fakeTime, BlockchainUpdateTriggers.noop)
      com.wavesplatform.checkGenesis(wavesSettings, blockchainUpdater, Miner.Disabled)
      sys.addShutdownHook(synchronized {
        blockchainUpdater.shutdown()
        rdb.close()
      })
      val (miner, appender) = createMinerAndAppender(blockchainUpdater, fakeTime)
      BlockchainObjects(blockchainUpdater, rdb, miner, appender, fakeTime)
    }

    def createChallenging(): BlockchainObjects = {
      val correctBlockchainDbDir = wavesSettings.dbSettings.directory + "/../challenged"
      FileUtils.copyDirectory(new File(wavesSettings.dbSettings.directory), new File(correctBlockchainDbDir))
      val dbSettings         = wavesSettings.dbSettings.copy(directory = correctBlockchainDbDir)
      val fixedWavesSettings = wavesSettings.copy(dbSettings = dbSettings)
      val rdb                = RDB.open(dbSettings)
      val rocksDBWriter      = new RocksDBWriter(rdb, fixedWavesSettings.blockchainSettings, fixedWavesSettings.dbSettings)
      val fakeTime           = createFakeTime(rocksDBWriter.lastBlockTimestamp.get)
      val blockchainUpdater = new BlockchainUpdaterImpl(
        rocksDBWriter,
        fixedWavesSettings,
        fakeTime,
        BlockchainUpdateTriggers.noop,
        (minHeight, maxHeight) => loadActiveLeases(rdb, minHeight, maxHeight)
      ) {
        override def balanceSnapshots(address: Address, from: Int, to: Option[BlockId]): Seq[BalanceSnapshot] = {
          val initSnapshots = super.balanceSnapshots(address, from, to)

          if (address == maliciousMiner.toAddress) {
            initSnapshots.map { bs =>
              bs.copy(leaseOut = bs.regularBalance)
            }
          } else if (address == challengingMiner.toAddress) {
            initSnapshots.map { bs =>
              bs.copy(leaseIn = super.balance(maliciousMiner.toAddress))
            }
          } else {
            initSnapshots
          }
        }
      }

      com.wavesplatform.checkGenesis(fixedWavesSettings, blockchainUpdater, Miner.Disabled)
      sys.addShutdownHook(synchronized {
        blockchainUpdater.shutdown()
        rdb.close()
      })

      val (miner, appender) = createMinerAndAppender(blockchainUpdater, fakeTime)
      BlockchainObjects(blockchainUpdater, rdb, miner, appender, fakeTime)
    }

    private def createMinerAndAppender(
        blockchain: BlockchainUpdaterImpl,
        fakeTime: Time
    ): (MinerImpl, Block => Task[Either[ValidationError, Option[BigInt]]]) = {
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
      val blockAppender = BlockAppender(blockchain, fakeTime, utx, posSelector, scheduler, verify = false) _

      miner -> blockAppender
    }

    private def createFakeTime(startTime: Long) =
      new Time {
        @volatile
        var time: Long = startTime

        override def correctedTime(): Long = time
        override def getTimestamp(): Long  = time
      }
  }
}
