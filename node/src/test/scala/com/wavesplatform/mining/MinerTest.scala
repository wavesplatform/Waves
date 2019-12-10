package com.wavesplatform.mining

import com.typesafe.config.ConfigFactory
import com.wavesplatform.account.{KeyPair, PublicKey}
import com.wavesplatform.block.Block
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.db.WithDomain
import com.wavesplatform.history.Domain
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{Constants, FunctionalitySettings, WalletSettings, WavesSettings}
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import com.wavesplatform.{TransactionGen, WithDB}
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import org.scalatest.{EitherValues, FreeSpec, Matchers, PrivateMethodTester}

class MinerTest extends FreeSpec with Matchers with WithDB with TransactionGen with EitherValues with PrivateMethodTester with WithDomain {
  import MinerTest._

  "Miner should calculate next block delay" - {
    "when rollback happened" in {
      withEnv {
        case Env(Domain(bcu, _), minerAcc, nextBlockGenerationTime) =>
          val genesis = TestBlock.create(
            ntpNow,
            Seq(GenesisTransaction.create(minerAcc.publicKey.toAddress, Constants.TotalWaves * Constants.UnitsInWave, ntpNow).right.value)
          )
          val b1 = TestBlock.create(ntpNow, genesis.uniqueId, Seq(), signer = minerAcc)
          val b2 = TestBlock.create(ntpNow, b1.uniqueId, Seq(), signer = minerAcc)
          bcu.processBlock(genesis).right.value
          bcu.processBlock(b1).right.value
          bcu.processBlock(b2).right.value

          nextBlockGenerationTime(Settings.blockchainSettings.functionalitySettings, bcu.height, b2, minerAcc.publicKey) shouldBe 'right
          bcu.removeAfter(b1.uniqueId).right.value should contain theSameElementsAs Seq(b2)
          nextBlockGenerationTime(Settings.blockchainSettings.functionalitySettings, bcu.height, b2, minerAcc.publicKey) shouldBe 'left
      }
    }
  }

  private def withEnv(f: Env => Unit): Unit = {
    withDomain(Settings) {
      case d @ Domain(blockchainUpdater, _) =>
        val allChannels                 = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
        val wallet                      = Wallet(WalletSettings(None, Some("123"), None))
        val utx                         = new UtxPoolImpl(ntpTime, blockchainUpdater, ignoreSpendableBalanceChanged, Settings.utxSettings)
        val scheduler: SchedulerService = Scheduler.singleThread("appender")
        val pos                         = new PoSSelector(blockchainUpdater, Settings.blockchainSettings, Settings.synchronizationSettings)
        val minerAcc                    = wallet.generateNewAccount().get
        val miner                       = new MinerImpl(allChannels, blockchainUpdater, Settings, ntpTime, utx, wallet, pos, scheduler, scheduler)

        val nextBlockGenerationTime = (fs: FunctionalitySettings, height: Int, lastBlock: Block, minerAcc: PublicKey) => {
          val call = PrivateMethod[Either[String, Long]]('nextBlockGenerationTime)
          miner invokePrivate call(fs, height, lastBlock, minerAcc)
        }

        val env = Env(d, minerAcc, nextBlockGenerationTime)

        f(env)
    }
  }
}

object MinerTest {
  private val DefaultSettings = WavesSettings.fromRootConfig(ConfigFactory.load())
  private val Settings        = DefaultSettings.copy(minerSettings = DefaultSettings.minerSettings.copy(quorum = 0))

  private case class Env(
      domain: Domain,
      minerAcc: KeyPair,
      nextBlockGenerationTime: (FunctionalitySettings, Int, Block, PublicKey) => Either[String, Long]
  )
}
