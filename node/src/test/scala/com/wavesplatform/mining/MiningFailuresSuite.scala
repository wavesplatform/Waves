package com.wavesplatform.mining

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.KeyPair
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings._
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.{BalanceSnapshot, BlockMinerInfo, NG}
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.transaction.TxValidationError.BlockFromFuture
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{TransactionGen, WithDB}
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}

class MiningFailuresSuite extends FlatSpec with Matchers with PrivateMethodTester with PathMockFactory with WithDB with TransactionGen {
  trait BlockchainUpdaterNG extends BlockchainUpdater with NG

  behavior of "Miner"

  it should "generate valid blocks ignoring time errors " in {
    val blockchainUpdater = stub[BlockchainUpdaterNG]

    val wavesSettings = {
      val config = ConfigFactory.parseString("""
          |waves.miner {
          |  quorum = 0
          |  interval-after-last-block-then-generation-is-allowed = 0
          |}""".stripMargin).withFallback(ConfigFactory.load())

      WavesSettings.fromRootConfig(loadConfig(config))
    }

    val miner = {
      val scheduler   = Scheduler.singleThread("appender")
      val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
      val wallet      = Wallet(WalletSettings(None, Some("123"), None))
      val utxPool     = new UtxPoolImpl(ntpTime, blockchainUpdater, ignoreSpendableBalanceChanged, wavesSettings.utxSettings, enablePriorityPool = true)
      val pos         = new PoSSelector(blockchainUpdater, wavesSettings.blockchainSettings, wavesSettings.synchronizationSettings)
      new MinerImpl(allChannels, blockchainUpdater, wavesSettings, ntpTime, utxPool, wallet, pos, scheduler, scheduler)
    }

    val genesis = TestBlock.create(System.currentTimeMillis(), Nil)
    (blockchainUpdater.isLastBlockId _).when(genesis.uniqueId).returning(true)
    (blockchainUpdater.heightOf _).when(genesis.uniqueId).returning(Some(1))
    (blockchainUpdater.height _).when().returning(1)
    (blockchainUpdater.settings _).when().returning(wavesSettings.blockchainSettings)
    (blockchainUpdater.lastBlock _).when().returning(Some(genesis))
    (blockchainUpdater.parentHeader _).when(*, *).returning(Some(genesis.getHeader()))
    (blockchainUpdater.activatedFeatures _).when().returning(Map.empty)
    (blockchainUpdater.bestLastBlockInfo _)
      .when(*)
      .returning(
        Some(
          BlockMinerInfo(
            NxtLikeConsensusBlockData(genesis.consensusData.baseTarget, genesis.consensusData.generationSignature),
            genesis.timestamp,
            genesis.uniqueId
          )
        )
      )
    (blockchainUpdater.processBlock _).when(*, *).returning(Left(BlockFromFuture(100))).repeated(10)
    (blockchainUpdater.processBlock _).when(*, *).returning(Right(None)).once()
    (blockchainUpdater.balanceSnapshots _).when(*, *, *).returning(Seq(BalanceSnapshot(1, ENOUGH_AMT, 0, 0)))

    val account       = accountGen.sample.get
    val generateBlock = generateBlockTask(miner)(account)
    generateBlock.runSyncUnsafe() shouldBe ((): Unit)
  }

  private[this] def generateBlockTask(miner: MinerImpl)(account: KeyPair): Task[Unit] =
    miner.invokePrivate(PrivateMethod[Task[Unit]]('generateBlockTask)(account))
}
