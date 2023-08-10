package com.wavesplatform.mining

import com.typesafe.config.ConfigFactory
import com.wavesplatform.WithDB
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.{Block, SignedBlockHeader}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.*
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult.Applied
import com.wavesplatform.state.{BalanceSnapshot, BlockMinerInfo, Blockchain, NG}
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.transaction.TxValidationError.BlockFromFuture
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable
import org.scalamock.scalatest.PathMockFactory

class MiningFailuresSuite extends FlatSpec with PathMockFactory with WithDB {
  trait BlockchainUpdaterNG extends Blockchain with BlockchainUpdater with NG

  behavior of "Miner"

  it should "generate valid blocks ignoring time errors " in {
    val blockchainUpdater = stub[BlockchainUpdaterNG]

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

    val miner = {
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
        wallet,
        pos,
        scheduler,
        scheduler,
        Observable.empty
      )
    }

    val genesis = TestBlock.create(System.currentTimeMillis(), Nil)
    (blockchainUpdater.isLastBlockId _).when(genesis.id()).returning(true)
    (blockchainUpdater.heightOf _).when(genesis.id()).returning(Some(1)).anyNumberOfTimes()
    (blockchainUpdater.heightOf _).when(genesis.header.reference).returning(Some(1)).anyNumberOfTimes()
    (() => blockchainUpdater.height).when().returning(1)
    (() => blockchainUpdater.settings).when().returning(blockchainSettings)
    (blockchainUpdater.blockHeader _).when(*).returns(Some(SignedBlockHeader(genesis.header, genesis.signature)))
    (() => blockchainUpdater.activatedFeatures).when().returning(Map.empty)
    (() => blockchainUpdater.approvedFeatures).when().returning(Map.empty)
    (blockchainUpdater.hitSource _).when(*).returns(Some(ByteStr(new Array[Byte](32))))
    (blockchainUpdater.bestLastBlockInfo _)
      .when(*)
      .returning(
        Some(
          BlockMinerInfo(
            genesis.header.baseTarget,
            genesis.header.generationSignature,
            genesis.header.timestamp,
            genesis.id()
          )
        )
      )

    var minedBlock: Block = null
    (blockchainUpdater.processBlock _).when(*, *, *).returning(Left(BlockFromFuture(100))).repeated(10)
    (blockchainUpdater.processBlock _)
      .when(*, *, *)
      .onCall { (block, _, _) =>
        minedBlock = block
        Right(Applied(Nil, 0))
      }
      .once()
    (blockchainUpdater.balanceSnapshots _).when(*, *, *).returning(Seq(BalanceSnapshot(1, ENOUGH_AMT, 0, 0)))

    val account       = accountGen.sample.get
    val generateBlock = generateBlockTask(miner)(account)
    generateBlock.runSyncUnsafe() shouldBe ((): Unit)
    minedBlock.header.featureVotes shouldBe empty
  }

  private[this] def generateBlockTask(miner: MinerImpl)(account: KeyPair): Task[Unit] =
    miner.generateBlockTask(account, None)
}
