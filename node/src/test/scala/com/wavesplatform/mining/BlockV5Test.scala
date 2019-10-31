package com.wavesplatform.mining

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{Constants, FunctionalitySettings, WalletSettings, WavesSettings}
import com.wavesplatform.state.NG
import com.wavesplatform.transaction.{BlockchainUpdater, GenesisTransaction}
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._

class BlockV5Test
    extends FlatSpec
    with ScalaCheckPropertyChecks
    with WithDomain
    with Matchers
    with TransactionGen
    with NoShrink
    with OptionValues
    with EitherValues
    with PrivateMethodTester {
  import BlockV5Test._

  private val activationScenario = for {
    (miner, b1) <- genesis
    b2 = TestBlock.create(ntpNow, b1.uniqueId, Seq.empty, miner, version = Block.PlainBlockVersion)
    b3 = TestBlock.create(ntpNow, b2.uniqueId, Seq.empty, miner, version = Block.PlainBlockVersion)
    b4 = TestBlock.create(ntpNow, b3.uniqueId, Seq.empty, miner, version = Block.NgBlockVersion)
    b5 = TestBlock.create(ntpNow, b4.uniqueId, Seq.empty, miner, version = Block.RewardBlockVersion, rewardVote = 7 * Constants.UnitsInWave)
  } yield (miner, Seq(b1, b2, b3, b4, b5))

  "Miner" should "generate valid blocks" in forAll(activationScenario) {
    case (minerAcc, bs) =>
      withBlockchain { blockchain =>
        bs.foreach(blockchain.processBlock(_).explicitGet())
        blockchain.height shouldBe bs.size
        blockchain.lastBlock.value.header.version shouldBe Block.RewardBlockVersion
        withMiner(blockchain) {
          case (miner, time) =>
            time.advance(10.minute)

            val forgedAtActivationHeight = miner invokePrivate forgeBlock(minerAcc)
            forgedAtActivationHeight shouldBe 'right
            val blockAtActivationHeight = forgedAtActivationHeight.right.value._2
            blockAtActivationHeight.header.version shouldBe Block.RewardBlockVersion

            blockchain.processBlock(blockAtActivationHeight).explicitGet()
            blockchain.height shouldBe bs.size + 1
            blockchain.lastBlock.value.header.version shouldBe Block.RewardBlockVersion
            blockAtActivationHeight.signature shouldBe blockchain.lastBlock.value.signature

            time.advance(10.minute)

            val forgedAfterActivationHeight = miner invokePrivate forgeBlock(minerAcc)
            forgedAfterActivationHeight shouldBe 'right
            val blockAfterActivationHeight = forgedAfterActivationHeight.right.value._2
            blockAfterActivationHeight.header.version shouldBe Block.ProtoBlockVersion

            blockchain.processBlock(blockAfterActivationHeight).explicitGet()
            blockchain.height shouldBe bs.size + 2
            blockchain.lastBlock.value.header.version shouldBe Block.ProtoBlockVersion
            blockAfterActivationHeight.signature shouldBe blockchain.lastBlock.value.signature

            time.advance(10.minute)

            val forgedAfterVRFUsing = miner invokePrivate forgeBlock(minerAcc)
            forgedAfterVRFUsing shouldBe 'right
            val blockAfterVRFUsing = forgedAfterVRFUsing.right.value._2
            blockAfterVRFUsing.header.version shouldBe Block.ProtoBlockVersion

            blockchain.processBlock(blockAfterVRFUsing).explicitGet()
            blockchain.height shouldBe bs.size + 3
            blockchain.lastBlock.value.header.version shouldBe Block.ProtoBlockVersion
            blockAfterVRFUsing.signature shouldBe blockchain.lastBlock.value.signature
        }
      }
  }

  private val forgeBlock = PrivateMethod[Either[String, (MiningConstraints, Block, MiningConstraint)]]('forgeBlock)

  private def genesis: Gen[(KeyPair, Block)] =
    for {
      miner <- accountGen
      genesisBlock = TestBlock.create(
        time = ntpNow,
        ref = TestBlock.randomSignature(),
        signer = TestBlock.defaultSigner,
        txs = Seq(
          GenesisTransaction.create(miner, Constants.TotalWaves * Constants.UnitsInWave, ntpNow).explicitGet()
        ),
        version = Block.GenesisBlockVersion
      )
    } yield (miner, genesisBlock)

  private def withBlockchain(f: BlockchainUpdater with NG => Unit): Unit =
    withDomain(testSettings) { d =>
      f(d.blockchainUpdater)
    }

  private def withMiner(blockchain: BlockchainUpdater with NG)(f: (MinerImpl, TestTime) => Unit): Unit = {
    val pos                         = new PoSSelector(blockchain, testSettings.blockchainSettings, testSettings.synchronizationSettings)
    val allChannels                 = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val wallet                      = Wallet(WalletSettings(None, Some("123"), None))
    val utxPool                     = new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, testSettings.utxSettings)
    val scheduler: SchedulerService = Scheduler.singleThread("appender")
    val time                        = new TestTime(ntpTime.correctedTime())
    val miner                       = new MinerImpl(allChannels, blockchain, testSettings, time, utxPool, wallet, pos, scheduler, scheduler)
    f(miner, time)
  }
}

object BlockV5Test {
  private val BlockV5ActivationHeight     = 6
  private val FairPoSActivationHeight     = 5
  private val BlockRewardActivationHeight = 4
  private val NGActivationHeight          = 3

  private val defaultSettings = WavesSettings.fromRootConfig(ConfigFactory.load())
  private val testSettings = defaultSettings.copy(
    blockchainSettings = defaultSettings.blockchainSettings.copy(
      functionalitySettings = FunctionalitySettings(
        featureCheckBlocksPeriod = 10,
        blocksForFeatureActivation = 1,
        doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
        blockVersion3AfterHeight = NGActivationHeight,
        preActivatedFeatures = Map(
          BlockchainFeatures.BlockV5.id     -> BlockV5ActivationHeight,
          BlockchainFeatures.BlockReward.id -> BlockRewardActivationHeight,
          BlockchainFeatures.NG.id          -> NGActivationHeight,
          BlockchainFeatures.FairPoS.id     -> FairPoSActivationHeight
        )
      )
    ),
    minerSettings = defaultSettings.minerSettings.copy(quorum = 0)
  )
}
