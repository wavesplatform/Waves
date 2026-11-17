package com.wavesplatform.mining

import com.wavesplatform.block.Block
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.{WalletSettings, WavesSettings}
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.{BlockEndorser, Blockchain, EndorsementStorage, NG, appender}
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

trait WithMiner extends WithDomain { suite: Suite =>
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
    val miner = new MinerImpl(
      channels,
      blockchain,
      settings,
      time,
      utxPool,
      BlockEndorser.Disabled,
      EndorsementStorage.Disabled,
      wallet,
      pos,
      minerScheduler,
      appenderScheduler,
      Observable(),
      timeDrift
    )
    def appendBlock(b: Block) = {
      val appendTask = BlockAppender(blockchain, time, utxPool, pos, BlockEndorser.Disabled, appenderScheduler, verify)(b, None)
      Await.result(appendTask.runToFuture(using appenderScheduler), Inf)
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

  /** A domain whose BlockchainUpdater schedules mining on a MinerImpl that runs on the given (usually Test-) schedulers. Mining accounts are the
    * first `minerAccounts` of the domain wallet.
    */
  def withDomainAndTestMiner(
      settings: WavesSettings,
      balances: Seq[AddrWithBalance],
      minerScheduler: Scheduler,
      appenderScheduler: Scheduler,
      utxEvents: Observable[Unit] = Observable.never, // The microblock miner waits on it when the UTX is empty; Observable.empty would fail it
      minerAccounts: Int = 1
  )(f: (Domain, MinerImpl) => Unit): Unit = {
    val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    var miner    = Miner.StrictDisabledMiner
    try
      withDomain(settings, balances, miner = Miner.forwardTo(miner)) { d =>
        d.wallet.generateNewAccounts(minerAccounts)
        // The genesis timestamp comes from the JVM-wide TxHelpers counter, which can lag the wall clock (TestTime's start) by minutes
        // in a full run; a block then gets "now - 1 minute" as its timestamp and the next attempt fires at once
        d.testTime.setTime(d.lastBlock.header.timestamp)
        val minerImpl = new MinerImpl(
          channels,
          d.blockchain,
          d.settings,
          d.testTime,
          d.utxPool,
          BlockEndorser.Disabled,
          EndorsementStorage.Disabled,
          d.wallet,
          d.posSelector,
          minerScheduler,
          appenderScheduler,
          utxEvents
        )
        miner = minerImpl
        f(d, minerImpl)
      }
    finally channels.close()
  }
}
