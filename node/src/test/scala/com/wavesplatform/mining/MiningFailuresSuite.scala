package com.wavesplatform.mining

import java.io.{PrintWriter, StringWriter}

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
import com.wavesplatform.settings._
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{BlockchainUpdaterImpl, NG}
import com.wavesplatform.transaction.{Asset, BlockchainUpdater, GenesisTransaction, Transaction}
import com.wavesplatform.utils.Time
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

class MiningFailuresSuite extends AsyncFlatSpec with Matchers with WithDB with TransactionGen with PrivateMethodTester with DBCacheSettings {
  import MiningFailuresSuite._

  behavior of "Miner"

  it should "generate valid blocks ignoring time errors " in {
    withEnv(Seq.empty) {
      case Env(_, account, miner, blockchain, fakeTime) =>
        val generateBlock = generateBlockTask(miner)(account)
        fakeTime.fromFutureGenEnabled = true
        Scheduler.global.scheduleOnce(3 seconds)(fakeTime.fromFutureGenEnabled = false)
        for {
          _ <- generateBlock
        } yield blockchain.height should be(2)
    }
  }

  private def withEnv(bps: Seq[BlockProducer], txs: Seq[TransactionProducer] = Seq(), times: Seq[Long] = Seq(System.currentTimeMillis()))(
      f: Env => Task[Assertion]
  ): Task[Assertion] =
    resources.use {
      case (blockchainUpdater, _, fakeTime) =>
        for {
          _ <- Task.unit
          pos          = new PoSSelector(blockchainUpdater, blockchainSettings, synchronizationSettings)
          utxPool      = new UtxPoolImpl(fakeTime, blockchainUpdater, ignoreSpendableBalanceChanged, utxSettings)
          scheduler    = Scheduler.singleThread("appender")
          allChannels  = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
          wallet       = Wallet(WalletSettings(None, Some("123"), None))
          miner        = new MinerImpl(allChannels, blockchainUpdater, settings, fakeTime, utxPool, wallet, pos, scheduler, scheduler)
          account      = createAccount
          ts           = fakeTime.correctedTime() - 60000
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
          env = Env(blocks, account, miner, blockchainUpdater, fakeTime)
          r <- f(env)
        } yield r
    }

  private def generateBlockTask(miner: MinerImpl)(account: KeyPair): Task[Unit] =
    miner.invokePrivate(PrivateMethod[Task[Unit]]('generateBlockTask)(account))

  private def resources: Resource[Task, (BlockchainUpdaterImpl, DB, FakeTimeService)] =
    Resource.make {
      val fakeTime          = new FakeTimeService
      val defaultWriter     = new LevelDBWriter(db, ignoreSpendableBalanceChanged, blockchainSettings, dbSettings)
      val blockchainUpdater = new BlockchainUpdaterImpl(defaultWriter, ignoreSpendableBalanceChanged, settings, fakeTime)
      Task.now((blockchainUpdater, db, fakeTime))
    } {
      case (blockchainUpdater, db, _) =>
        Task {
          blockchainUpdater.shutdown()
          db.close()
        }
    }
}

object MiningFailuresSuite {
  class FakeTimeService extends Time {
    @volatile
    var fromFutureGenEnabled = false

    private[this] def nextTime(): Long = {
      val stackTrace = {
        val err = new RuntimeException()
        val sw  = new StringWriter()
        val pw  = new PrintWriter(sw)
        err.printStackTrace(pw)
        pw.close()
        sw.toString
      }

      val shouldPatch = stackTrace.contains("com.wavesplatform.mining.MinerImpl.currentTime$1")
      val time        = System.currentTimeMillis()
      if (shouldPatch && fromFutureGenEnabled) time + 1000000 else time
    }

    override def correctedTime(): Long = nextTime()
    override def getTimestamp(): Long  = nextTime()
  }

  import TestFunctionalitySettings.Enabled
  import monix.execution.Scheduler.Implicits.global

  type BlockProducer       = (Long, ByteStr, KeyPair) => Block
  type TransactionProducer = (Long, KeyPair) => Transaction

  case class Env(blocks: Seq[Block], account: KeyPair, miner: MinerImpl, blockchain: BlockchainUpdater with NG, time: FakeTimeService)

  val commonSettings: WavesSettings                    = WavesSettings.fromRootConfig(loadConfig(ConfigFactory.load()))
  val minerSettings: MinerSettings                     = commonSettings.minerSettings.copy(quorum = 0, intervalAfterLastBlockThenGenerationIsAllowed = 1 hour)
  val synchronizationSettings: SynchronizationSettings = commonSettings.synchronizationSettings
  val utxSettings: UtxSettings                         = commonSettings.utxSettings

  val functionalitySettings: FunctionalitySettings =
    Enabled
      .copy(preActivatedFeatures = Enabled.preActivatedFeatures + (BlockchainFeatures.BlockReward.id -> 0))
  val blockchainSettings: BlockchainSettings =
    commonSettings.blockchainSettings
      .copy(functionalitySettings = functionalitySettings)
      .copy(rewardsSettings = RewardsSettings.TESTNET)

  val settings: WavesSettings = commonSettings.copy(minerSettings = minerSettings, blockchainSettings = blockchainSettings)

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
