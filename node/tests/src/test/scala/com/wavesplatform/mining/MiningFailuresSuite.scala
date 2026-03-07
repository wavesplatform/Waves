package com.wavesplatform.mining

import com.typesafe.config.ConfigFactory
import com.wavesplatform.WithNewDBForEachTest
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.api.BlockMeta
import com.wavesplatform.block.{Block, BlockSnapshot, MicroBlock, MicroBlockSnapshot, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.*
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult.Applied
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.*
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.{BlockchainUpdater, DiscardedBlocks, LastBlockInfo, Transaction}
import com.wavesplatform.transaction.TxValidationError.BlockFromFuture
import com.wavesplatform.utils.EmptyBlockchain
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.AtomicInt
import monix.reactive.Observable

class MiningFailuresSuite extends FlatSpec, WithNewDBForEachTest {
  trait BlockchainUpdaterNG extends Blockchain with BlockchainUpdater with NG

  behavior of "Miner"

  it should "generate valid blocks ignoring time errors " in {
    @volatile var minedBlock: Block = null
    val genesis                     = TestBlock.create(System.currentTimeMillis(), Nil).block
    val blockchainUpdater = new EmptyBlockchain with BlockchainUpdater with NG {
      override def height: Int = 1

      override def heightOf(blockId: ByteStr): Option[Int] = Some(1)

      override def hitSource(height: Int): Option[ByteStr] = Some(ByteStr(new Array[Byte](32)))

      override def blockHeader(height: Int): Option[SignedBlockHeader] = Some(SignedBlockHeader(genesis.header, genesis.signature))

      override def balanceSnapshots(address: Address, from: Int, to: Option[ByteStr]): Seq[BalanceSnapshot] =
        Seq(BalanceSnapshot(Height(1), ENOUGH_AMT, 0, 0, 0))

      override def bestLastBlockInfo(maxMicroblockTimestampMs: Long): Option[BlockMinerInfo] = Some(
        BlockMinerInfo(
          genesis.header.baseTarget,
          genesis.header.generationSignature,
          genesis.header.timestamp,
          genesis.id()
        )
      )

      override def isLastBlockId(id: ByteStr) = id == genesis.id() || Option(minedBlock).map(_.id()).contains(id)

      private val counter = AtomicInt(0)

      override def processBlock(
          block: Block,
          hitSource: ByteStr,
          snapshot: Option[BlockSnapshot],
          generatorSet: GeneratorSet,
          challengedHitSource: Option[ByteStr],
          verify: Boolean,
          txSignParCheck: Boolean
      ): Either[ValidationError, BlockchainUpdaterImpl.BlockApplyResult] =
        if (counter.getAndIncrement() >= 9) {
          minedBlock = block
          Right(Applied(Nil, 0, Seq.empty))
        } else
          Left(BlockFromFuture(100, 100))

      override def processMicroBlock(
          microBlock: MicroBlock,
          snapshot: Option[MicroBlockSnapshot],
          verify: Boolean
      ): Either[ValidationError, Block.BlockId] = ???

      override def computeNextReward: Option[Long] = Some(0)

      override def removeAfter(blockId: ByteStr): Either[ValidationError, DiscardedBlocks] = Right(Seq.empty)

      override def lastBlockInfo: Observable[LastBlockInfo] = Observable.empty

      override def referencedBlockchain(reference: ByteStr): Blockchain = this

      override def shutdown(): Unit = {}

      override def microBlock(id: ByteStr): Option[MicroBlock] = None

      override def microblockIds: Seq[Block.BlockId] = Seq.empty

      override def liquidBlock(id: ByteStr): Option[Block] = None

      override def liquidBlockSnapshot(id: ByteStr): Option[StateSnapshot] = None

      override def microBlockSnapshot(totalBlockId: ByteStr): Option[StateSnapshot] = None

      override def liquidTransactions(id: ByteStr): Option[Seq[(TxMeta, Transaction)]] = None

      override def liquidBlockMeta: Option[BlockMeta] = None

      override def bestLiquidSnapshot: Option[StateSnapshot] = None

      override def bestLiquidSnapshotAndFees: Option[(StateSnapshot, Long, Long)] = None

      override def snapshotBlockchain: SnapshotBlockchain = ???

      override def currentGeneratorSet: Option[GeneratorSet] = ???
    }

    val wavesSettings = {
      val config = ConfigFactory
        .parseString("""
                       |waves.miner {
                       |  quorum = 0
                       |  interval-after-last-block-then-generation-is-allowed = 0
                       |}
                       |
                       |waves.features.supported=[2]
                       |""".stripMargin)
        .withFallback(ConfigFactory.load())

      WavesSettings.fromRootConfig(loadConfig(config))
    }

    val blockchainSettings = {
      val bs = wavesSettings.blockchainSettings
      val fs = bs.functionalitySettings
      bs.copy(functionalitySettings = fs.copy(blockVersion3AfterHeight = 0, preActivatedFeatures = Map(2.toShort -> 0)))
    }

    val (miner, appenderScheduler) = {
      val scheduler   = Scheduler.singleThread("appender")
      val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
      val wallet      = Wallet(WalletSettings(None, Some("123"), None))
      val utxPool =
        new UtxPoolImpl(ntpTime, blockchainUpdater, wavesSettings.utxSettings, wavesSettings.maxTxErrorLogSize, wavesSettings.minerSettings.enable)
      val pos = PoSSelector(blockchainUpdater, wavesSettings.synchronizationSettings.maxBaseTarget)
      new MinerImpl(
        allChannels,
        blockchainUpdater,
        wavesSettings.copy(blockchainSettings = blockchainSettings),
        ntpTime,
        utxPool,
        BlockEndorser.Disabled,
        EndorsementStorage.Disabled,
        wallet,
        pos,
        scheduler,
        scheduler,
        Observable.empty
      ) -> scheduler
    }

    val account       = accountGen.sample.get
    val generateBlock = generateBlockTask(miner)(account)
    generateBlock.runSyncUnsafe() shouldBe ((): Unit)
    minedBlock.header.featureVotes shouldBe empty
    appenderScheduler.shutdown()
  }

  private def generateBlockTask(miner: MinerImpl)(account: KeyPair): Task[Unit] =
    miner.generateBlockTask(account, None)
}
