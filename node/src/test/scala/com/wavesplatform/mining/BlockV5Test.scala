package com.wavesplatform.mining

import java.util.concurrent.atomic.AtomicReference
import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{AddressOrAlias, KeyPair}
import com.wavesplatform.block.serialization.{BlockHeaderSerializer, BlockSerializer}
import com.wavesplatform.block.validation.Validators
import com.wavesplatform.block.{Block, BlockHeader, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.{chainBaseAndMicro, defaultSigner}
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.protobuf.block.PBBlocks
import com.wavesplatform.settings.{Constants, FunctionalitySettings, TestFunctionalitySettings, WalletSettings, WavesSettings}
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult.Applied
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.{Blockchain, BlockchainUpdaterImpl, NG, diffs}
import com.wavesplatform.test.{FlatSpec, *}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{BlockchainUpdater, GenesisTransaction, Transaction, TxHelpers, TxVersion}
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{BlocksTransactionsHelpers, crypto, protobuf}
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalacheck.Gen
import org.scalatest.*
import org.scalatest.enablers.Length

import scala.concurrent.Await
import scala.concurrent.duration.*
class BlockV5Test extends FlatSpec with WithDomain with OptionValues with EitherValues with BlocksTransactionsHelpers {
  import BlockV5Test.*

  private val testTime = TestTime(1)
  def shiftTime(miner: MinerImpl, minerAcc: KeyPair): Unit = {
    val offset = miner.getNextBlockGenerationOffset(minerAcc).explicitGet()
    testTime.advance(offset + 1.milli)
  }

  "Proto block" should "be serialized" in {
    val stateHash = ByteStr.fill(DigestLength)(1)
    val features  = Seq(534, 3, 33, 5, 1, 0, 12343242).map(_.toShort)
    val block =
      Block
        .buildAndSign(
          Block.ProtoBlockVersion,
          System.currentTimeMillis(),
          TestBlock.randomSignature(),
          2L,
          ByteStr(Array.fill(Block.GenerationVRFSignatureLength)(0: Byte)),
          Seq.empty,
          defaultSigner,
          features.sorted,
          -1,
          Some(stateHash),
          None
        )
        .explicitGet()

    def updateHeader(block: Block, f: BlockHeader => BlockHeader): Block =
      block.copy(header = f(block.header))

    val blockWithBadVotes = updateHeader(block, _.copy(featureVotes = features))

    withClue("validations") {
      Validators.validateBlock(blockWithBadVotes).left.value
      Validators.validateBlock(block).explicitGet()
      Validators
        .validateBlock(
          updateHeader(block, _.copy(version = Block.NgBlockVersion))
        )
        .left
        .value
      Validators
        .validateBlock(
          updateHeader(block, _.copy(generationSignature = ByteStr(new Array[Byte](32))))
        )
        .left
        .value
      Validators
        .validateBlock(
          updateHeader(block, _.copy(featureVotes = Seq[Short](1, 1, 1)))
        )
        .left
        .value
      Validators
        .validateBlock(
          updateHeader(blockWithBadVotes, _.copy(version = Block.NgBlockVersion, generationSignature = ByteStr(new Array[Byte](32))))
        )
        .explicitGet()
      Validators.validateBlock(updateHeader(block, _.copy(stateHash = Some(ByteStr.fill(DigestLength - 1)(1))))).left.value
    }

    withClue("preserve feature order") {
      val serialized1        = BlockSerializer.toBytes(blockWithBadVotes)
      val deserialized1      = PBBlocks.vanilla(protobuf.block.PBBlock.parseFrom(serialized1)).get
      val serialized2        = PBBlocks.protobuf(blockWithBadVotes).toByteArray
      val deserialized2      = PBBlocks.vanilla(protobuf.block.PBBlock.parseFrom(serialized2)).get
      val serializedHeader   = BlockHeaderSerializer.toBytes(blockWithBadVotes.header)
      val deserializedHeader = PBBlocks.vanilla(protobuf.block.PBBlockHeader.parseFrom(serializedHeader))
      serialized1 shouldBe serialized2
      all(Seq(deserialized1, deserialized2)) should matchPattern {
        case b: Block if !b.signatureValid() => // Pass
      }
      all(Seq(deserialized1, deserialized2)) shouldBe blockWithBadVotes
      deserializedHeader shouldBe blockWithBadVotes.header
    }

    withClue("signature valid") {
      val serialized1        = BlockSerializer.toBytes(block)
      val deserialized1      = PBBlocks.vanilla(protobuf.block.PBBlock.parseFrom(serialized1)).get
      val serialized2        = PBBlocks.protobuf(block).toByteArray
      val deserialized2      = PBBlocks.vanilla(protobuf.block.PBBlock.parseFrom(serialized2)).get
      val serializedHeader   = BlockHeaderSerializer.toBytes(block.header)
      val deserializedHeader = PBBlocks.vanilla(protobuf.block.PBBlockHeader.parseFrom(serializedHeader))
      serialized1 shouldBe serialized2
      all(Seq(deserialized1, deserialized2)) should matchPattern {
        case b: Block if b.signatureValid() => // Pass
      }
      all(Seq(deserialized1, deserialized2)) shouldBe block
      deserializedHeader shouldBe block.header
    }
  }

  "Miner" should "generate valid blocks" in forAll(genesis) { case (minerAcc1, minerAcc2, genesis) =>
    val disabledFeatures = new AtomicReference(Set[Short]())
    withBlockchain(disabledFeatures, testTime) { blockchain =>
      blockchain.processBlock(genesis, genesis.header.generationSignature) should beRight
      withMiner(blockchain, testTime) { case (miner, appender, scheduler) =>
        for (h <- 2 until BlockV5ActivationHeight) {

          shiftTime(miner, minerAcc1)

          val forge = miner.forgeBlock(minerAcc1)
          val block = forge.explicitGet()._1
          Await.result(appender(block).runToFuture(scheduler), 10.seconds).explicitGet() shouldBe an[Applied]
          blockchain.height shouldBe h
        }

        blockchain.height shouldBe BlockV5ActivationHeight - 1

        shiftTime(miner, minerAcc2)

        val forgedAtActivationHeight = miner.forgeBlock(minerAcc2)
        val blockAtActivationHeight  = forgedAtActivationHeight.explicitGet()._1
        blockAtActivationHeight.header.version shouldBe Block.ProtoBlockVersion

        Await.result(appender(blockAtActivationHeight).runToFuture(scheduler), 10.seconds).explicitGet() shouldBe an[Applied]
        blockchain.height shouldBe BlockV5ActivationHeight
        blockchain.lastBlockHeader.value.header.version shouldBe Block.ProtoBlockVersion
        blockAtActivationHeight.signature shouldBe blockchain.lastBlockHeader.value.signature

        val hitSourceBeforeActivationHeight = blockchain.hitSource(BlockV5ActivationHeight - 1).get
        hitSourceBeforeActivationHeight shouldBe blockchain.blockHeader(BlockV5ActivationHeight - 1).get.header.generationSignature

        val hitSourceAtActivationHeight = blockchain.hitSource(BlockV5ActivationHeight).get
        hitSourceAtActivationHeight shouldBe crypto
          .verifyVRF(
            blockAtActivationHeight.header.generationSignature,
            hitSourceBeforeActivationHeight.arr,
            minerAcc2.publicKey
          )
          .explicitGet()

        shiftTime(miner, minerAcc1)

        val forgedAfterActivationHeight = miner.forgeBlock(minerAcc1)
        val blockAfterActivationHeight  = forgedAfterActivationHeight.explicitGet()._1
        blockAfterActivationHeight.header.version shouldBe Block.ProtoBlockVersion

        Await.result(appender(blockAfterActivationHeight).runToFuture(scheduler), 10.seconds).explicitGet() shouldBe an[Applied]
        blockchain.height shouldBe BlockV5ActivationHeight + 1
        blockchain.lastBlockHeader.value.header.version shouldBe Block.ProtoBlockVersion
        blockAfterActivationHeight.signature shouldBe blockchain.lastBlockHeader.value.signature

        val hitSourceAfterActivationHeight = blockchain.hitSource(BlockV5ActivationHeight + 1).get
        hitSourceAfterActivationHeight shouldBe crypto
          .verifyVRF(
            blockAfterActivationHeight.header.generationSignature,
            hitSourceAtActivationHeight.arr,
            minerAcc1.publicKey
          )
          .explicitGet()

        shiftTime(miner, minerAcc2)

        val forgedAfterVRFUsing = miner.forgeBlock(minerAcc2)
        val blockAfterVRFUsing  = forgedAfterVRFUsing.explicitGet()._1
        blockAfterVRFUsing.header.version shouldBe Block.ProtoBlockVersion

        Await.result(appender(blockAfterVRFUsing).runToFuture(scheduler), 10.seconds).explicitGet() shouldBe an[Applied]
        blockchain.height shouldBe BlockV5ActivationHeight + 2
        blockchain.lastBlockHeader.value.header.version shouldBe Block.ProtoBlockVersion
        blockAfterVRFUsing.signature shouldBe blockchain.lastBlockHeader.value.signature

        val hitSourceAfterVRFUsing = blockchain.hitSource(BlockV5ActivationHeight + 2).get
        hitSourceAfterVRFUsing shouldBe crypto
          .verifyVRF(
            blockAfterVRFUsing.header.generationSignature,
            hitSourceAfterActivationHeight.arr,
            minerAcc2.publicKey
          )
          .explicitGet()

        blockchain.blockHeader(BlockV5ActivationHeight).value.signature shouldBe blockAtActivationHeight.signature
        blockchain.blockHeader(BlockV5ActivationHeight + 1).value.signature shouldBe blockAfterActivationHeight.signature
        blockchain.blockHeader(BlockV5ActivationHeight + 2).value.signature shouldBe blockAfterVRFUsing.signature

        blockchain.parentHeader(blockAfterVRFUsing.header).value shouldBe blockAfterActivationHeight.header
        blockchain.parentHeader(blockAfterVRFUsing.header, 2).value shouldBe blockAtActivationHeight.header

        disabledFeatures.set(Set(BlockchainFeatures.BlockV5.id))

        shiftTime(miner, minerAcc2)

        val oldVersionBlockForge = miner.forgeBlock(minerAcc2)
        val oldVersionBlock      = oldVersionBlockForge.explicitGet()._1
        oldVersionBlock.header.version shouldBe Block.RewardBlockVersion

        disabledFeatures.set(Set())
        Await.result(appender(oldVersionBlock).runToFuture(scheduler), 10.seconds).left.value

        for (h <- blockchain.height to 110) {

          shiftTime(miner, minerAcc1)

          val forged = miner.forgeBlock(minerAcc1)
          val block  = forged.explicitGet()._1
          block.header.version shouldBe Block.ProtoBlockVersion

          Await.result(appender(block).runToFuture(scheduler), 10.seconds).explicitGet() shouldBe an[Applied]
          blockchain.height shouldBe (h + 1)

          val hitSource     = blockchain.hitSource(if (h > 100) h - 100 else h).value
          val nextHitSource = blockchain.hitSource(h + 1).value
          val lastBlock     = blockchain.lastBlockHeader.value

          nextHitSource shouldBe crypto
            .verifyVRF(
              lastBlock.header.generationSignature,
              hitSource.arr,
              minerAcc1.publicKey
            )
            .explicitGet()
        }
      }
    }
  }

  "Miner" should "generate valid blocks when feature pre-activated" in forAll(genesis) { case (minerAcc1, _, genesis) =>
    withBlockchain(new AtomicReference(Set()), testTime, preActivatedTestSettings) { blockchain =>
      blockchain.processBlock(genesis, genesis.header.generationSignature) should beRight
      withMiner(blockchain, testTime) { case (miner, appender, scheduler) =>
        for (h <- blockchain.height to 110) {

          shiftTime(miner, minerAcc1)

          val forged = miner.forgeBlock(minerAcc1)
          val block  = forged.explicitGet()._1
          block.header.version shouldBe Block.ProtoBlockVersion

          Await.result(appender(block).runToFuture(scheduler), 10.seconds).explicitGet() shouldBe an[Applied]
          blockchain.height shouldBe (h + 1)
        }
      }
    }
  }

  "Block version" should "be validated accordingly features activation" in forAll(genesis) { case (minerAcc, _, genesis) =>
    val disabledFeatures = new AtomicReference(Set.empty[Short])
    withBlockchain(disabledFeatures, testTime) { blockchain =>
      blockchain.processBlock(genesis, genesis.header.generationSignature) should beRight
      withMiner(blockchain, testTime) { case (miner, appender, scheduler) =>
        def forge(): Block = {
          val forge = miner.forgeBlock(minerAcc)
          forge.explicitGet()._1
        }

        def forgeAppendAndValidate(version: Byte, height: Int): Unit = {
          val block = forge()
          block.header.version shouldBe version
          Await.result(appender(block).runToFuture(scheduler), 10.seconds).explicitGet() shouldBe an[Applied]
          blockchain.height shouldBe height
        }

        for (h <- 2 to NGActivationHeight + 1) {
          shiftTime(miner, minerAcc)
          forgeAppendAndValidate(Block.PlainBlockVersion, h)
        }

        shiftTime(miner, minerAcc)
        forgeAppendAndValidate(Block.NgBlockVersion, NGActivationHeight + 2)

        for (h <- blockchain.height + 1 to BlockRewardActivationHeight) {
          shiftTime(miner, minerAcc)
          forgeAppendAndValidate(Block.NgBlockVersion, h)
        }

        disabledFeatures.set(Set(BlockchainFeatures.BlockReward.id))
        shiftTime(miner, minerAcc)
        val badNgBlock = forge()
        badNgBlock.header.version shouldBe Block.NgBlockVersion
        disabledFeatures.set(Set())
        Await.result(appender(badNgBlock).runToFuture(scheduler), 10.seconds).left.value

        shiftTime(miner, minerAcc)
        forgeAppendAndValidate(Block.RewardBlockVersion, BlockRewardActivationHeight + 1)

        for (h <- blockchain.height + 1 until BlockV5ActivationHeight) {
          shiftTime(miner, minerAcc)
          forgeAppendAndValidate(Block.RewardBlockVersion, h)
        }

        disabledFeatures.set(Set(BlockchainFeatures.BlockV5.id))
        shiftTime(miner, minerAcc)
        val badRewardBlock = forge()
        badRewardBlock.header.version shouldBe Block.RewardBlockVersion
        disabledFeatures.set(Set())
        Await.result(appender(badNgBlock).runToFuture(scheduler), 10.seconds).left.value
        Await.result(appender(badRewardBlock).runToFuture(scheduler), 10.seconds).left.value

        shiftTime(miner, minerAcc)
        forgeAppendAndValidate(Block.ProtoBlockVersion, BlockV5ActivationHeight)
      }
    }
  }

  private def createTx(sender: KeyPair, recipient: AddressOrAlias): Transaction =
    TransferTransaction
      .selfSigned(TxVersion.V1, sender, recipient, Waves, 10 * Constants.UnitsInWave, Waves, 100000, ByteStr.empty, ntpTime.getTimestamp())
      .explicitGet()

  private val updaterScenario = for {
    (miner1, miner2, b1) <- genesis
    b2        = TestBlock.create(ntpNow, b1.id(), Seq.empty, miner1, version = Block.PlainBlockVersion).block
    b3        = TestBlock.create(ntpNow, b2.id(), Seq.empty, miner1, version = Block.NgBlockVersion).block
    tx1       = createTx(miner1, miner2.toAddress)
    tx2       = createTx(miner2, miner1.toAddress)
    tx3       = createTx(miner1, miner2.toAddress)
    tx4       = createTx(miner2, miner1.toAddress)
    tx5       = createTx(miner1, miner2.toAddress)
    (b4, m4s) = chainBaseAndMicro(b3.id(), Seq.empty, Seq(Seq(tx1)), miner2, Block.NgBlockVersion, ntpNow)
    (b5, m5s) = chainBaseAndMicro(m4s.head.totalBlockId, Seq.empty, Seq(Seq(tx2)), miner1, Block.RewardBlockVersion, ntpNow)
    (b6, m6s) = chainBaseAndMicro(m5s.head.totalBlockId, Seq(tx3), Seq(Seq(tx4)), miner2, Block.ProtoBlockVersion, ntpNow)
    (b7, m7s) = chainBaseAndMicro(m6s.head.totalBlockId, Seq.empty, Seq(Seq(tx5)), miner1, Block.ProtoBlockVersion, ntpNow)
  } yield (Seq(b1, b2, b3), (b4, m4s), (b5, m5s), (b6, m6s), (b7, m7s))

  "BlockchainUpdater" should "accept valid key blocks and microblocks" in forAll(updaterScenario) {
    case (bs, (ngBlock, ngMicros), (rewardBlock, rewardMicros), (protoBlock, protoMicros), (afterProtoBlock, afterProtoMicros)) =>
      withBlockchain(new AtomicReference(Set())) { blockchain =>
        bs.foreach(b => blockchain.processBlock(b, b.header.generationSignature) should beRight)

        blockchain.processBlock(ngBlock, ngBlock.header.generationSignature) should beRight
        ngMicros.foreach(m => blockchain.processMicroBlock(m) should beRight)

        blockchain.processBlock(rewardBlock, rewardBlock.header.generationSignature) should beRight
        rewardMicros.foreach(m => blockchain.processMicroBlock(m) should beRight)

        blockchain.processBlock(protoBlock, protoBlock.header.generationSignature) should beRight
        protoMicros.foreach(m => blockchain.processMicroBlock(m) should beRight)

        blockchain.processBlock(afterProtoBlock, afterProtoBlock.header.generationSignature) should beRight
        afterProtoMicros.foreach(m => blockchain.processMicroBlock(m) should beRight)
      }
  }

  "blockId" should "be header hash" in {
    implicit val byteStrLength: Length[ByteStr] = (obj: ByteStr) => obj.arr.length

    val preconditions = for {
      acc <- accountGen
      ts      = System.currentTimeMillis()
      genesis = GenesisTransaction.create(acc.toAddress, diffs.ENOUGH_AMT, ts).explicitGet()
    } yield (acc, genesis)

    forAll(preconditions) { case (acc, genesis) =>
      val fs = TestFunctionalitySettings.Stub.copy(preActivatedFeatures =
        Map(
          BlockchainFeatures.NG.id            -> 0,
          BlockchainFeatures.BlockV5.id       -> 2,
          BlockchainFeatures.SmartAccounts.id -> 0
        )
      )

      withDomain(WavesSettings.default().copy(blockchainSettings = WavesSettings.default().blockchainSettings.copy(functionalitySettings = fs))) {
        d =>
          def applyBlock(txs: Transaction*): SignedBlockHeader = {
            d.appendBlock(
              if (d.blockchainUpdater.height >= 1) Block.ProtoBlockVersion else Block.PlainBlockVersion,
              txs*
            )
            lastBlock
          }

          def lastBlock: SignedBlockHeader =
            d.blockchainUpdater.lastBlockHeader.get

          val block1 = applyBlock(genesis, TxHelpers.genesis(TxHelpers.defaultAddress)) // h=1
          block1.id() shouldBe block1.signature
          block1.id() should have length crypto.SignatureLength

          val block2 = applyBlock() // h=2
          block2.header.reference shouldBe block1.signature
          block2.id() should have length crypto.DigestLength

          val block3 = applyBlock()
          block3.header.reference shouldBe block2.id()
          block3.id() should have length crypto.DigestLength

          val keyBlock = d.appendKeyBlock()
          val mb1      = d.createMicroBlock()(TxHelpers.transfer())
          d.blockchain.processMicroBlock(mb1)
          d.appendMicroBlock(TxHelpers.transfer())

          mb1.totalResBlockSig should have length crypto.SignatureLength
          mb1.reference should not be keyBlock.signature
          mb1.reference shouldBe keyBlock.id()
      }
    }
  }

  private def genesis: Gen[(KeyPair, KeyPair, Block)] =
    for {
      miner1 <- accountGen
      miner2 <- accountGen
      genesisBlock = TestBlock
        .create(
          time = ntpNow,
          ref = TestBlock.randomSignature(),
          signer = TestBlock.defaultSigner,
          txs = Seq(
            GenesisTransaction.create(miner1.toAddress, Constants.TotalWaves / 2 * Constants.UnitsInWave, ntpNow).explicitGet(),
            GenesisTransaction.create(miner2.toAddress, Constants.TotalWaves / 2 * Constants.UnitsInWave, ntpNow).explicitGet()
          ),
          version = Block.GenesisBlockVersion
        )
        .block
    } yield (miner1, miner2, genesisBlock)

  private def withBlockchain(disabledFeatures: AtomicReference[Set[Short]], time: Time = ntpTime, settings: WavesSettings = testSettings)(
      f: Blockchain & BlockchainUpdater & NG => Unit
  ): Unit = {
    withRocksDBWriter(settings.blockchainSettings) { blockchain =>
      val bcu: BlockchainUpdaterImpl =
        new BlockchainUpdaterImpl(blockchain, settings, time, ignoreBlockchainUpdateTriggers, (_, _) => Seq.empty) {
          override def activatedFeatures: Map[Short, Int] = super.activatedFeatures -- disabledFeatures.get()
        }
      try f(bcu)
      finally bcu.shutdown()
    }
  }

  type Appender = Block => Task[Either[ValidationError, BlockApplyResult]]

  private def withMiner(blockchain: Blockchain & BlockchainUpdater & NG, time: Time, settings: WavesSettings = testSettings)(
      f: (MinerImpl, Appender, Scheduler) => Unit
  ): Unit = {
    val pos               = PoSSelector(blockchain, settings.synchronizationSettings.maxBaseTarget)
    val allChannels       = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val wallet            = Wallet(WalletSettings(None, Some("123"), None))
    val utxPool           = new UtxPoolImpl(time, blockchain, settings.utxSettings, settings.maxTxErrorLogSize, settings.minerSettings.enable)
    val minerScheduler    = Scheduler.singleThread("miner")
    val appenderScheduler = Scheduler.singleThread("appender")
    val miner = new MinerImpl(allChannels, blockchain, settings, time, utxPool, wallet, pos, minerScheduler, appenderScheduler, Observable.empty)
    val blockAppender = BlockAppender(blockchain, time, utxPool, pos, appenderScheduler) _
    f(miner, blockAppender, appenderScheduler)
    appenderScheduler.shutdown()
    minerScheduler.shutdown()
    utxPool.close()
  }
}

object BlockV5Test {
  private val BlockV5ActivationHeight     = 11
  private val FairPoSActivationHeight     = 10
  private val BlockRewardActivationHeight = 6
  private val NGActivationHeight          = 3

  private val defaultSettings = WavesSettings.fromRootConfig(ConfigFactory.load())
  private val testSettings = defaultSettings.copy(
    blockchainSettings = defaultSettings.blockchainSettings.copy(
      functionalitySettings = FunctionalitySettings(
        featureCheckBlocksPeriod = 10,
        blocksForFeatureActivation = 1,
        blockVersion3AfterHeight = NGActivationHeight,
        preActivatedFeatures = Map(
          BlockchainFeatures.BlockV5.id     -> BlockV5ActivationHeight,
          BlockchainFeatures.BlockReward.id -> BlockRewardActivationHeight,
          BlockchainFeatures.NG.id          -> NGActivationHeight,
          BlockchainFeatures.FairPoS.id     -> FairPoSActivationHeight
        ),
        doubleFeaturesPeriodsAfterHeight = Int.MaxValue
      )
    ),
    minerSettings = defaultSettings.minerSettings.copy(quorum = 0)
  )
  private val preActivatedTestSettings = testSettings.copy(
    blockchainSettings = testSettings.blockchainSettings.copy(
      functionalitySettings = testSettings.blockchainSettings.functionalitySettings.copy(
        blockVersion3AfterHeight = 0,
        preActivatedFeatures = Map(
          BlockchainFeatures.BlockV5.id     -> 0,
          BlockchainFeatures.BlockReward.id -> 0,
          BlockchainFeatures.NG.id          -> 0,
          BlockchainFeatures.FairPoS.id     -> 0
        )
      )
    )
  )
}
