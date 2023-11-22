package com.wavesplatform

import com.wavesplatform.block.Block
import com.wavesplatform.consensus.PoSSelector
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.{WalletSettings, WavesSettings}
import com.wavesplatform.state.BlockchainUpdaterImpl.BlockApplyResult
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.state.{Blockchain, NG, StateSnapshot, appender}
import com.wavesplatform.transaction.{BlockchainUpdater, Transaction}
import com.wavesplatform.utils.Time
import com.wavesplatform.utx.UtxPoolImpl
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.Scheduler
import monix.reactive.Observable

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

package object mining {
  private[mining] def createConstConstraint(maxSize: Long, transactionSize: => Long, description: String) = OneDimensionalMiningConstraint(
    maxSize,
    new com.wavesplatform.mining.TxEstimators.Fn {
      override def apply(b: Blockchain, t: Transaction, s: StateSnapshot): Long = transactionSize
      override val minEstimate                                                  = transactionSize
      override val toString: String                                             = s"const($transactionSize)"
    },
    description
  )

  type Appender = Block => Either[ValidationError, BlockApplyResult]

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
      Await.result(appendTask.runToFuture(appenderScheduler), 10.seconds)
    }
    f(miner, appendBlock)
    appenderScheduler.shutdown()
    minerScheduler.shutdown()
    utxPool.close()
  }
}
