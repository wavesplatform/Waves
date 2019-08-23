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
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{
  BlockRewardSettings,
  BlockchainSettings,
  DBSettings,
  FunctionalitySettings,
  MinerSettings,
  SynchronizationSettings,
  TestFunctionalitySettings,
  UtxSettings,
  WalletSettings,
  WavesSettings,
  loadConfig
}
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{BlockchainUpdaterImpl, NG}
import com.wavesplatform.transaction.{Asset, BlockchainUpdater, GenesisTransaction}
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

//  it should "generate valid empty blocks for version 4" in {
//    withEnv(Seq.empty) {
//      case Env(_, account, miner, blockchain) =>
//        val generateBlock = generateBlockTask(miner)(account)
//        val oldBalance    = blockchain.balance(account)
//        val newBalance    = oldBalance + 2 * settings.blockchainSettings.functionalitySettings.blockRewardSettings.firstReward
//        for {
//          _ <- generateBlock
//          _ <- generateBlock
//        } yield {
//          blockchain.balance(account) should be(newBalance)
//          blockchain.height should be(3)
//          blockchain.blockAt(2).get.version should be(Block.RewardBlockVersion)
//          blockchain.blockAt(3).get.version should be(Block.RewardBlockVersion)
//        }
//    }
//  }

//  it should "test" in {
//    withEnv(Seq((ts, reference) => TestBlock.create(time = ts, ref = reference, txs = Seq.empty))) {
//      case Env(_, account, miner, blockchain) =>
//        val generateBlock = generateBlockTask(miner)(account)
//        val oldBalance    = blockchain.balance(account)
//        val newBalance    = oldBalance + settings.blockchainSettings.functionalitySettings.blockRewardSettings.firstReward
//
//        generateBlock.map { _ =>
//          blockchain.balance(account) should be(newBalance)
//          blockchain.height should be(3)
//        }
//    }
//  }

  private def generateBlockTask(miner: MinerImpl)(account: KeyPair): Task[Unit] =
    miner.invokePrivate(PrivateMethod[Task[Unit]]('generateBlockTask)(account))

  private def withEnv(bps: Seq[BlockProducer])(f: Env => Task[Assertion]): Task[Assertion] =
    resources.use {
      case (blockchainUpdater, _) =>
        for {
          _ <- Task.unit
          pos          = new PoSSelector(blockchainUpdater, blockchainSettings, synchronizationSettings)
          utxPool      = new UtxPoolImpl(ntpTime, blockchainUpdater, ignoreSpendableBalanceChanged, utxSettings)
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
              (ts + 3, bp(ts + 3, chain.head.uniqueId) +: chain)
          }._2
          added <- Task.traverse(blocks)(b => Task(blockchainUpdater.processBlock(b)))
          _ = added.foreach(_.explicitGet())
          env = Env(blocks, account, miner, blockchainUpdater)
          r <- f(env)
        } yield r
    }

  private def resources: Resource[Task, (BlockchainUpdater with NG, DB)] =
    Resource.make {
      val defaultWriter: LevelDbWriterWithReward       = new LevelDbWriterWithReward(db, ignoreSpendableBalanceChanged, blockchainSettings, dbSettings)
      val blockchainUpdater: BlockchainUpdater with NG = new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, settings, ntpTime)
      defaultWriter.saveReward(settings.blockchainSettings.functionalitySettings.blockRewardSettings.firstReward)
      Task.now(blockchainUpdater, db)
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

  type BlockProducer = (Long, ByteStr) => Block

  case class Env(blocks: Seq[Block], account: KeyPair, miner: MinerImpl, blockchain: BlockchainUpdater with NG)

  val functionalitySettings: FunctionalitySettings =
    Enabled
      .copy(preActivatedFeatures = Enabled.preActivatedFeatures + (BlockchainFeatures.BlockReward.id -> 0))
      .copy(blockVersion4AfterHeight = 0)
      .copy(blockRewardSettings = BlockRewardSettings.TESTNET)

  val commonSettings: WavesSettings                    = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))
  val minerSettings: MinerSettings                     = commonSettings.minerSettings.copy(quorum = 0, intervalAfterLastBlockThenGenerationIsAllowed = 1 hour)
  val blockchainSettings: BlockchainSettings           = commonSettings.blockchainSettings.copy(functionalitySettings = functionalitySettings)
  val synchronizationSettings: SynchronizationSettings = commonSettings.synchronizationSettings
  val utxSettings: UtxSettings                         = commonSettings.utxSettings
  val settings: WavesSettings                          = commonSettings.copy(minerSettings = minerSettings, blockchainSettings = blockchainSettings)

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
      blockRewardCache = Some(newReward)
      db.put(Keys.blockReward.keyBytes, Keys.blockReward.encode(blockRewardCache))
    }
  }

  private implicit def taskToFuture(task: Task[Assertion]): Future[Assertion] = task.runToFuture
}
