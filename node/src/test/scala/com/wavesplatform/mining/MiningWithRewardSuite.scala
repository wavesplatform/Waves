package com.wavesplatform.mining

import cats.effect.Resource
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.database.{Keys, LevelDBWriter}
import com.wavesplatform.db.DBCacheSettings
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings._
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{BlockchainUpdaterImpl, NG}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import com.wavesplatform.transaction.{Asset, BlockchainUpdater, GenesisTransaction, Transaction}
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{TransactionGen, WithDB}
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observer
import org.iq80.leveldb.DB
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.compatible.Assertion
import org.scalatest.{AsyncFlatSpec, Matchers, PrivateMethodTester}

import scala.concurrent.Future
import scala.concurrent.duration._

class MiningWithRewardSuite extends AsyncFlatSpec with Matchers with WithDB with TransactionGen with PrivateMethodTester with DBCacheSettings {
  import MiningWithRewardSuite._

  behavior of "Miner with activated reward feature"

  it should "generate valid empty blocks of version 4" in {
    withEnv(Seq.empty) {
      case Env(_, account, miner, blockchain) =>
        val generateBlock = generateBlockTask(miner)(account)
        val oldBalance    = blockchain.balance(account)
        val newBalance    = oldBalance + 2 * settings.blockchainSettings.rewardsSettings.initial
        for {
          _ <- generateBlock
          _ <- generateBlock
        } yield {
          blockchain.balance(account) should be(newBalance)
          blockchain.height should be(3)
          blockchain.blockAt(2).get.version should be(Block.RewardBlockVersion)
          blockchain.blockAt(3).get.version should be(Block.RewardBlockVersion)
        }
    }
  }

  it should "generate valid empty block of version 4 after block of version 3" in {
    withEnv(Seq((ts, reference, _) => TestBlock.create(time = ts, ref = reference, txs = Seq.empty, version = Block.NgBlockVersion))) {
      case Env(_, account, miner, blockchain) =>
        val generateBlock = generateBlockTask(miner)(account)
        val oldBalance    = blockchain.balance(account)
        val newBalance    = oldBalance + settings.blockchainSettings.rewardsSettings.initial

        generateBlock.map { _ =>
          blockchain.balance(account) should be(newBalance)
          blockchain.height should be(3)
        }
    }
  }

  it should "generate valid blocks with transactions of version 4" in {
    val bps: Seq[BlockProducer] = Seq(
      (ts, reference, account) => {
        val recipient1 = createAccount.toAddress
        val recipient2 = createAccount.toAddress
        val tx1 = TransferTransactionV2
          .selfSigned(Waves, account, recipient1, 10 * Constants.UnitsInWave, ts, Waves, 400000, Array())
          .explicitGet()
        val tx2 = TransferTransactionV2
          .selfSigned(Waves, account, recipient2, 5 * Constants.UnitsInWave, ts, Waves, 400000, Array())
          .explicitGet()
        TestBlock.create(time = ts, ref = reference, txs = Seq(tx1, tx2), version = Block.NgBlockVersion)
      }
    )

    val txs: Seq[TransactionProducer] = Seq(
      (ts, account) => {
        val recipient1 = createAccount.toAddress
        TransferTransactionV2
          .selfSigned(Waves, account, recipient1, 10 * Constants.UnitsInWave, ts, Waves, 400000, Array())
          .explicitGet()
      }
    )

    withEnv(bps, txs) {
      case Env(_, account, miner, blockchain) =>
        val generateBlock = generateBlockTask(miner)(account)
        val oldBalance    = blockchain.balance(account)
        val newBalance    = oldBalance + settings.blockchainSettings.rewardsSettings.initial - 10 * Constants.UnitsInWave

        generateBlock.map { _ =>
          blockchain.balance(account) should be(newBalance)
          blockchain.height should be(3)
        }
    }

    // Test for empty key block with NG
    withEnv(bps, txs, settingsWithFeatures(BlockchainFeatures.NG, BlockchainFeatures.SmartAccounts)) {
      case Env(_, account, miner, _) =>
        val (_, block, _) = forgeBlock(miner)(account).explicitGet()
        Task(block.transactionData shouldBe empty)
    }
  }

  private def withEnv(bps: Seq[BlockProducer], txs: Seq[TransactionProducer] = Seq(), settings: WavesSettings = MiningWithRewardSuite.settings)(
      f: Env => Task[Assertion]
  ): Task[Assertion] =
    resources(settings).use {
      case (blockchainUpdater, _) =>
        for {
          _ <- Task.unit
          pos          = new PoSSelector(blockchainUpdater, settings.blockchainSettings, settings.synchronizationSettings)
          utxPool      = new UtxPoolImpl(ntpTime, blockchainUpdater, ignoreSpendableBalanceChanged, settings.utxSettings, enablePriorityPool = true)
          scheduler    = Scheduler.singleThread("appender")
          allChannels  = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
          wallet       = Wallet(WalletSettings(None, Some("123"), None))
          miner        = new MinerImpl(allChannels, blockchainUpdater, settings, ntpTime, utxPool, wallet, pos, scheduler, scheduler)
          account      = createAccount
          ts           = ntpTime.correctedTime() - 60000
          genesisBlock = TestBlock.create(ts + 2, List(GenesisTransaction.create(account, ENOUGH_AMT, ts + 1).explicitGet()))
          _ <- Task(blockchainUpdater.processBlock(genesisBlock))
          blocks = bps.foldLeft {
            (ts + 1, Seq[Block](genesisBlock))
          } {
            case ((ts, chain), bp) =>
              (ts + 3, bp(ts + 3, chain.head.uniqueId, account) +: chain)
          }._2
          added <- Task.traverse(blocks.reverse)(b => Task(blockchainUpdater.processBlock(b)))
          _   = added.foreach(_.explicitGet())
          _   = txs.foreach(tx => utxPool.putIfNew(tx(ts + 6, account)).resultE.explicitGet())
          env = Env(blocks, account, miner, blockchainUpdater)
          r <- f(env)
        } yield r
    }

  private def generateBlockTask(miner: MinerImpl)(account: KeyPair): Task[Unit] =
    miner.invokePrivate(PrivateMethod[Task[Unit]]('generateBlockTask)(account))

  private def forgeBlock(miner: MinerImpl)(account: KeyPair): Either[String, (MiningConstraints, Block, MiningConstraint)] =
    miner.invokePrivate(PrivateMethod[Either[String, (MiningConstraints, Block, MiningConstraint)]]('forgeBlock)(account))

  private def resources(settings: WavesSettings): Resource[Task, (BlockchainUpdater with NG, DB)] =
    Resource.make {
      val defaultWriter: LevelDbWriterWithReward =
        new LevelDbWriterWithReward(db, ignoreSpendableBalanceChanged, settings.blockchainSettings, dbSettings)
      val blockchainUpdater: BlockchainUpdater with NG = new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, settings, ntpTime)
      defaultWriter.saveReward(settings.blockchainSettings.rewardsSettings.initial)
      Task.now((blockchainUpdater, db))
    } {
      case (blockchainUpdater, db) =>
        Task {
          blockchainUpdater.shutdown()
          db.close()
        }
    }
}

object MiningWithRewardSuite {
  import TestFunctionalitySettings.Enabled
  import monix.execution.Scheduler.Implicits.global

  type BlockProducer       = (Long, ByteStr, KeyPair) => Block
  type TransactionProducer = (Long, KeyPair) => Transaction

  case class Env(blocks: Seq[Block], account: KeyPair, miner: MinerImpl, blockchain: BlockchainUpdater with NG)

  val settings: WavesSettings = {
    val commonSettings: WavesSettings = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))
    val minerSettings: MinerSettings =
      commonSettings.minerSettings.copy(quorum = 0, intervalAfterLastBlockThenGenerationIsAllowed = 1 hour)

    val functionalitySettings: FunctionalitySettings =
      Enabled
        .copy(preActivatedFeatures = Enabled.preActivatedFeatures + (BlockchainFeatures.BlockReward.id -> 0))
    val blockchainSettings: BlockchainSettings =
      commonSettings.blockchainSettings
        .copy(functionalitySettings = functionalitySettings)
        .copy(rewardsSettings = RewardsSettings.TESTNET)
    commonSettings.copy(minerSettings = minerSettings, blockchainSettings = blockchainSettings)
  }

  def settingsWithFeatures(features: BlockchainFeature*): WavesSettings = {
    val blockchainSettings = settings.blockchainSettings

    settings.copy(
      blockchainSettings = blockchainSettings.copy(
        functionalitySettings = blockchainSettings.functionalitySettings.copy(preActivatedFeatures = features.map(_.id -> 0).toMap)
      )
    )
  }

  def createAccount: KeyPair =
    Gen
      .containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte])
      .map(bs => KeyPair(bs))
      .sample
      .get

  class LevelDbWriterWithReward(
      val db: DB,
      val spendableBalanceChanged: Observer[(Address, Asset)],
      override val settings: BlockchainSettings,
      override val dbSettings: DBSettings
  ) extends LevelDBWriter(db, spendableBalanceChanged, settings, dbSettings) {
    def saveReward(newReward: Long): Unit = {
      db.put(Keys.blockReward(0).keyBytes, Keys.blockReward(0).encode(Some(newReward)))
    }
  }

  private implicit def taskToFuture(task: Task[Assertion]): Future[Assertion] = task.runToFuture
}
