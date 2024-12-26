package com.wavesplatform.mining

import com.wavesplatform.block.Block
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.{WalletSettings, WavesSettings}
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.{Blockchain, NG, appender}
import com.wavesplatform.transaction.BlockchainUpdater
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalatest.Suite

import scala.concurrent.Await
import scala.concurrent.duration.Duration.Inf

trait WithMiner extends WithDomain { _: Suite =>
  def withMiner(
      blockchain: Blockchain & BlockchainUpdater & NG,
      time: Time,
      settings: WavesSettings,
      verify: Boolean = true,
      timeDrift: Long = appender.MaxTimeDrift
  )(
      f: (MinerImpl, Appender) => Unit
  ): Unit = {
    val pos               = PoSSelector(blockchain, settings.synchronizationSettings.maxBaseTarget)
    val channels          = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    val wallet            = Wallet(WalletSettings(None, Some("123"), None))
    val utxPool           = new UtxPoolImpl(time, blockchain, settings.utxSettings, settings.maxTxErrorLogSize, settings.minerSettings.enable)
    val minerScheduler    = Scheduler.singleThread("miner")
    val appenderScheduler = Scheduler.singleThread("appender")
    val miner = new MinerImpl(channels, blockchain, settings, time, utxPool, wallet, pos, minerScheduler, appenderScheduler, Observable(), timeDrift)
    def appendBlock(b: Block) = {
      val appendTask = BlockAppender(blockchain, time, utxPool, pos, appenderScheduler, verify)(b, None)
      Await.result(appendTask.runToFuture(appenderScheduler), Inf)
    }
    f(miner, appendBlock)
    appenderScheduler.shutdown()
    minerScheduler.shutdown()
    utxPool.close()
  }

  def withDomainAndMiner(
      settings: WavesSettings,
      balances: Seq[AddrWithBalance] = Seq(),
      verify: Boolean = true,
      timeDrift: Long = appender.MaxTimeDrift
  )(
      assert: (Domain, MinerImpl, Appender) => Unit
  ): Unit =
    withDomain(settings, balances) { d =>
      withMiner(d.blockchain, d.testTime, d.settings, verify, timeDrift)(assert(d, _, _))
    }
}
