package com.wavesplatform.mining

import com.typesafe.config.ConfigFactory
import com.wavesplatform.WithNewDBForEachTest
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.*
import com.wavesplatform.state.{Blockchain, NG}
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.eval.Task
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

class MiningFailuresSuite extends FlatSpec with WithNewDBForEachTest {
  trait BlockchainUpdaterNG extends Blockchain with BlockchainUpdater with NG

  behavior of "Miner"

  it should "generate valid blocks ignoring time errors " in {
    val blockchainUpdater: BlockchainUpdaterNG = ???

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
        wallet,
        pos,
        scheduler,
        scheduler,
        Observable.empty
      ) -> scheduler
    }

    val genesis = TestBlock.create(System.currentTimeMillis(), Nil).block

    var minedBlock: Block = null

    val account       = accountGen.sample.get
    val generateBlock = generateBlockTask(miner)(account)
    generateBlock.runSyncUnsafe() shouldBe ((): Unit)
    minedBlock.header.featureVotes shouldBe empty
    appenderScheduler.shutdown()
  }

  private[this] def generateBlockTask(miner: MinerImpl)(account: KeyPair): Task[Unit] =
    miner.generateBlockTask(account, None)
}
