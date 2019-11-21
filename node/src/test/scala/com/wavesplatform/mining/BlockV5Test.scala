package com.wavesplatform.mining

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{AddressOrAlias, KeyPair}
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.chainBaseAndMicro
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.{Constants, FunctionalitySettings, WalletSettings, WavesSettings}
import com.wavesplatform.state.NG
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.{Attachment, TransferTransaction}
import com.wavesplatform.transaction.{BlockchainUpdater, GenesisTransaction, Transaction, TxVersion}
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{NoShrink, TestTime, TransactionGen, crypto}
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.Await
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
    (miner1, miner2, b1) <- genesis
    b2 = TestBlock.create(ntpNow, b1.uniqueId, Seq.empty, miner1, version = Block.PlainBlockVersion)
    b3 = TestBlock.create(ntpNow, b2.uniqueId, Seq.empty, miner1, version = Block.PlainBlockVersion)
    b4 = TestBlock.create(ntpNow, b3.uniqueId, Seq.empty, miner1, version = Block.NgBlockVersion)
    b5 = TestBlock.create(ntpNow, b4.uniqueId, Seq.empty, miner1, version = Block.RewardBlockVersion, rewardVote = 7 * Constants.UnitsInWave)
  } yield (miner1, miner2, Seq(b1, b2, b3, b4, b5))

  "Miner" should "generate valid blocks" in forAll(activationScenario) {
    case (minerAcc1, minerAcc2, bs) =>
      withBlockchain { blockchain =>
        bs.foreach(b => blockchain.processBlock(b, b.header.generationSignature).explicitGet())
        blockchain.height shouldBe bs.size
        blockchain.lastBlock.value.header.version shouldBe Block.RewardBlockVersion
        withMiner(blockchain) {
          case (miner, time, appender, scheduler) =>
            time.setTime(ntpNow)
            time.advance(10.minute)

            val forgedAtActivationHeight = miner invokePrivate forgeBlock(minerAcc2)
            forgedAtActivationHeight shouldBe 'right
            val blockAtActivationHeight = forgedAtActivationHeight.right.value._2
            blockAtActivationHeight.header.version shouldBe Block.RewardBlockVersion

            Await.result(appender(blockAtActivationHeight).runToFuture(scheduler), 10.seconds).right.value shouldBe 'defined
            blockchain.height shouldBe bs.size + 1
            blockchain.lastBlock.value.header.version shouldBe Block.RewardBlockVersion
            blockAtActivationHeight.signature shouldBe blockchain.lastBlock.value.signature

            val hitSourceAtActivationHeight = blockchain.hitSourceAtHeight(bs.size + 1).get
            hitSourceAtActivationHeight shouldBe blockAtActivationHeight.header.generationSignature

            time.advance(10.minute)

            val forgedAfterActivationHeight = miner invokePrivate forgeBlock(minerAcc1)
            forgedAfterActivationHeight shouldBe 'right
            val blockAfterActivationHeight = forgedAfterActivationHeight.right.value._2
            blockAfterActivationHeight.header.version shouldBe Block.ProtoBlockVersion

            Await.result(appender(blockAfterActivationHeight).runToFuture(scheduler), 10.seconds).right.value shouldBe 'defined
            blockchain.height shouldBe bs.size + 2
            blockchain.lastBlock.value.header.version shouldBe Block.ProtoBlockVersion
            blockAfterActivationHeight.signature shouldBe blockchain.lastBlock.value.signature

            val hitSourceAfterActivationHeight = blockchain.hitSourceAtHeight(bs.size + 2).get
            hitSourceAfterActivationHeight shouldBe crypto
              .verifyVRF(
                blockAfterActivationHeight.header.generationSignature,
                hitSourceAtActivationHeight,
                minerAcc1.publicKey
              )
              .explicitGet()

            time.advance(10.minute)

            val forgedAfterVRFUsing = miner invokePrivate forgeBlock(minerAcc2)
            forgedAfterVRFUsing shouldBe 'right
            val blockAfterVRFUsing = forgedAfterVRFUsing.right.value._2
            blockAfterVRFUsing.header.version shouldBe Block.ProtoBlockVersion

            Await.result(appender(blockAfterVRFUsing).runToFuture(scheduler), 10.seconds).right.value shouldBe 'defined
            blockchain.height shouldBe bs.size + 3
            blockchain.lastBlock.value.header.version shouldBe Block.ProtoBlockVersion
            blockAfterVRFUsing.signature shouldBe blockchain.lastBlock.value.signature

            val hitSourceAfterVRFUsing = blockchain.hitSourceAtHeight(bs.size + 3).get
            hitSourceAfterVRFUsing shouldBe crypto
              .verifyVRF(
                blockAfterVRFUsing.header.generationSignature,
                hitSourceAfterActivationHeight,
                minerAcc2.publicKey
              )
              .explicitGet()

            blockchain.blockAt(bs.size + 1).value shouldBe blockAtActivationHeight
            blockchain.blockAt(bs.size + 2).value shouldBe blockAfterActivationHeight
            blockchain.blockAt(bs.size + 3).value shouldBe blockAfterVRFUsing

            blockchain.parentHeader(blockAfterVRFUsing.header).value shouldBe blockAfterActivationHeight.header
            blockchain.parentHeader(blockAfterVRFUsing.header, 2).value shouldBe blockAtActivationHeight.header
        }
      }
  }

  private def createTx(sender: KeyPair, recipient: AddressOrAlias): Transaction =
    TransferTransaction
      .selfSigned(TxVersion.V1, sender, recipient, Waves, 10 * Constants.UnitsInWave, Waves, 100000, Attachment.Empty, ntpTime.getTimestamp())
      .explicitGet()

  private val updaterScenario = for {
    (miner1, miner2, b1) <- genesis
    b2        = TestBlock.create(ntpNow, b1.uniqueId, Seq.empty, miner1, version = Block.PlainBlockVersion)
    b3        = TestBlock.create(ntpNow, b2.uniqueId, Seq.empty, miner1, version = Block.PlainBlockVersion)
    b4        = TestBlock.create(ntpNow, b3.uniqueId, Seq.empty, miner1, version = Block.NgBlockVersion)
    tx1       = createTx(miner1, miner2)
    tx2       = createTx(miner2, miner1)
    tx3       = createTx(miner1, miner2)
    tx4       = createTx(miner2, miner1)
    tx5       = createTx(miner1, miner2)
    (b5, m5s) = chainBaseAndMicro(b4.uniqueId, Seq.empty, Seq(Seq(tx1)), miner2, Block.NgBlockVersion, ntpNow)
    (b6, m6s) = chainBaseAndMicro(m5s.head.totalResBlockSig, Seq.empty, Seq(Seq(tx2)), miner1, Block.RewardBlockVersion, ntpNow)
    (b7, m7s) = chainBaseAndMicro(m6s.head.totalResBlockSig, Seq(tx3), Seq(Seq(tx4)), miner2, Block.ProtoBlockVersion, ntpNow)
    (b8, m8s) = chainBaseAndMicro(m7s.head.totalResBlockSig, Seq.empty, Seq(Seq(tx5)), miner1, Block.ProtoBlockVersion, ntpNow)
  } yield (Seq(b1, b2, b3, b4), (b5, m5s), (b6, m6s), (b7, m7s), (b8, m8s))

  "BlockchainUpdater" should "accept valid key blocks and microblocks" in forAll(updaterScenario) {
    case (bs, (ngBlock, ngMicros), (rewardBlock, rewardMicros), (protoBlock, protoMicros), (afterProtoBlock, afterProtoMicros)) =>
      withBlockchain { blockchain =>
        bs.foreach(b => blockchain.processBlock(b, b.header.generationSignature).explicitGet())

        blockchain.processBlock(ngBlock, ngBlock.header.generationSignature).explicitGet()
        ngMicros.foreach(m => blockchain.processMicroBlock(m).explicitGet())

        blockchain.processBlock(rewardBlock, rewardBlock.header.generationSignature).explicitGet()
        rewardMicros.foreach(m => blockchain.processMicroBlock(m).explicitGet())

        blockchain.processBlock(protoBlock, protoBlock.header.generationSignature).explicitGet()
        protoMicros.foreach(m => blockchain.processMicroBlock(m).explicitGet())

        blockchain.processBlock(afterProtoBlock, afterProtoBlock.header.generationSignature).explicitGet()
        afterProtoMicros.foreach(m => blockchain.processMicroBlock(m).explicitGet())
      }
  }

  private val forgeBlock = PrivateMethod[Either[String, (MiningConstraints, Block, MiningConstraint)]]('forgeBlock)

  private def genesis: Gen[(KeyPair, KeyPair, Block)] =
    for {
      miner1 <- accountGen
      miner2 <- accountGen
      genesisBlock = TestBlock.create(
        time = ntpNow,
        ref = TestBlock.randomSignature(),
        signer = TestBlock.defaultSigner,
        txs = Seq(
          GenesisTransaction.create(miner1, Constants.TotalWaves / 2 * Constants.UnitsInWave, ntpNow).explicitGet(),
          GenesisTransaction.create(miner2, Constants.TotalWaves / 2 * Constants.UnitsInWave, ntpNow).explicitGet()
        ),
        version = Block.GenesisBlockVersion
      )
    } yield (miner1, miner2, genesisBlock)

  private def withBlockchain(f: BlockchainUpdater with NG => Unit): Unit =
    withDomain(testSettings) { d =>
      f(d.blockchainUpdater)
    }

  type Appender = Block => Task[Either[ValidationError, Option[BigInt]]]

  private def withMiner(blockchain: BlockchainUpdater with NG)(f: (MinerImpl, TestTime, Appender, Scheduler) => Unit): Unit = {
    val pos               = new PoSSelector(blockchain, testSettings.blockchainSettings, testSettings.synchronizationSettings)
    val allChannels       = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val wallet            = Wallet(WalletSettings(None, Some("123"), None))
    val utxPool           = new UtxPoolImpl(ntpTime, blockchain, ignoreSpendableBalanceChanged, testSettings.utxSettings)
    val minerScheduler    = Scheduler.singleThread("miner")
    val appenderScheduler = Scheduler.singleThread("appender")
    val time              = new TestTime(ntpTime.correctedTime())
    val miner             = new MinerImpl(allChannels, blockchain, testSettings, time, utxPool, wallet, pos, minerScheduler, appenderScheduler)
    val blockAppender     = BlockAppender(blockchain, time, utxPool, pos, appenderScheduler) _
    f(miner, time, blockAppender, appenderScheduler)
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
