package com.wavesplatform.mining

import cats.syntax.option.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.state.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{CatchLogs, FreeSpec, NumericExt, TestSchedulerOps, TestTime, WithResourceManager}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import monix.reactive.subjects.ConcurrentSubject
import org.scalatest.EitherValues
import org.scalatest.time.SpanSugar.convertLongToGrainOfTime

import scala.util.Using

class LastMicroBlockSuite extends FreeSpec with WithDomain with TestSchedulerOps with WithResourceManager with EitherValues {
  private val thisNodeAcc1 = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 0)
  private val thisNodeAcc2 = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 1)
  private val otherNodeAcc = TxHelpers.defaultSigner

  private val baseSettings       = DomainPresets.TransactionStateSnapshot
  private val microBlockInterval = 5.seconds
  private val minMicroBlockAge   = 3.seconds
  private val defaultSettings = baseSettings.copy(
    minerSettings = baseSettings.minerSettings.copy(quorum = 0, microBlockInterval = microBlockInterval, minMicroBlockAge = minMicroBlockAge)
  )

  "Same node accounts - next account mining with minMicroblockAge" in Using.Manager { manager =>
    val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    manager.acquire(channels)(using _.close())

    var miner = Miner.StrictDisabledMiner
    val time  = TestTime()
    withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(thisNodeAcc1, thisNodeAcc2, otherNodeAcc),
      miner = Miner.forwardTo(miner),
      time = time
    ) { d =>
      val minerScheduler    = TestScheduler()
      val appenderScheduler = TestScheduler()

      d.wallet.generateNewAccounts(2)

      miner = new MinerImpl(
        channels,
        d.blockchain,
        d.settings,
        time,
        d.utxPool,
        BlockEndorser.Disabled,
        EndorsementStorage.Disabled,
        d.wallet,
        d.posSelector,
        minerScheduler,
        appenderScheduler,
        Observable.empty
      )

      log.debug("Append block2")
      val block2 = d.createBlock(generator = thisNodeAcc1, strictTime = true)
      d.appender.appendBlock(block2)
      time.setTime(block2.header.timestamp)
      appenderScheduler.tickNext("this-appender-1", failIfNoTasks = false)

      log.debug("Append microBlock1")
      time.advance(microBlockInterval)
      val microBlock1 = d.createMicroBlock(signer = thisNodeAcc1.some)(TxHelpers.transfer(from = otherNodeAcc, to = thisNodeAcc2.toAddress))
      d.appendMicroBlock(microBlock1)
      val refLiquidBlockId = d.lastBlockId
      appenderScheduler.tickNext("this-appender-2", failIfNoTasks = false)

      log.debug("Append microBlock2")
      time.advance(microBlockInterval)
      d.appendMicroBlock(d.createMicroBlock(signer = thisNodeAcc1.some)(TxHelpers.transfer(from = otherNodeAcc, to = thisNodeAcc2.toAddress)))

      log.debug("Trigger thisNode forging")
      appenderScheduler.tickNext("this-appender-3")
      minerScheduler.tickNext("this-miner-1")
      appenderScheduler.tickNext("this-appender-4")

      val lastBlock = d.blockchain.lastBlockHeader.value
      lastBlock.header.reference shouldBe refLiquidBlockId
    }
  }.get

  "Different node accounts - next account mining without minMicroblockAge" in Using.Manager { manager =>
    val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    manager.acquire(channels)(using _.close())

    var miner = Miner.StrictDisabledMiner
    val time  = TestTime()
    withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(thisNodeAcc1, otherNodeAcc),
      miner = Miner.forwardTo(miner),
      time = time
    ) { d =>
      val minerScheduler    = TestScheduler()
      val appenderScheduler = TestScheduler()

      d.wallet.generateNewAccounts(1)

      miner = new MinerImpl(
        channels,
        d.blockchain,
        d.settings,
        time,
        d.utxPool,
        BlockEndorser.Disabled,
        EndorsementStorage.Disabled,
        d.wallet,
        d.posSelector,
        minerScheduler,
        appenderScheduler,
        Observable.empty
      )

      log.debug("Append block2")
      val block2 = d.createBlock(generator = otherNodeAcc, strictTime = true)
      d.appender.appendBlock(block2)
      time.setTime(block2.header.timestamp)
      appenderScheduler.tickNext("this-appender-1", failIfNoTasks = false)

      log.debug("Append microBlock1")
      time.advance(microBlockInterval)
      val microBlock1 = d.createMicroBlock(signer = otherNodeAcc.some)(TxHelpers.transfer(to = otherNodeAcc.toAddress))
      d.appendMicroBlock(microBlock1)
      appenderScheduler.tickNext("this-appender-2", failIfNoTasks = false)

      log.debug("Append microBlock2")
      time.advance(microBlockInterval)
      d.appendMicroBlock(d.createMicroBlock(signer = otherNodeAcc.some)(TxHelpers.transfer(to = otherNodeAcc.toAddress)))
      val refLiquidBlockId = d.lastBlockId

      log.debug("Trigger thisNode forging")
      appenderScheduler.tickNext("this-appender-3")
      minerScheduler.tickNext("this-miner-1")
      appenderScheduler.tickNext("this-appender-4")

      val lastBlock = d.blockchain.lastBlockHeader.value
      lastBlock.header.reference shouldBe refLiquidBlockId
    }
  }.get

  "transfer in the last microblock of period, but it removed" in withManager { manager =>
    val channels     = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))
    val thisNodeAcc2 = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 1)
    val thisNodeAcc3 = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 2)
    var miner        = Miner.StrictDisabledMiner

    withDomain(
      defaultSettings
        .configure(_.copy(generationPeriodLength = 3))
        .copy(minerSettings = defaultSettings.minerSettings.copy(maxTransactionsInMicroBlock = 2)), // Fixes 5 seconds delay
      AddrWithBalance.enoughBalances(otherNodeAcc, thisNodeAcc1, thisNodeAcc2) :+ AddrWithBalance(thisNodeAcc3.toAddress, 1000.waves - 1),
      miner = Miner.forwardTo(miner)
    ) { d =>
      val minerScheduler    = TestScheduler()
      val appenderScheduler = TestScheduler()
      d.wallet.generateNewAccounts(3)

      val utxEvents = ConcurrentSubject.publish[Unit](using minerScheduler)
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
      ) with CatchLogs
      miner = minerImpl

      d.appender.appendBlock(d.createBlock(strictTime = true, generator = otherNodeAcc))

      log.debug("Trigger forging of block 3")
      val blockTs3 = Seq(thisNodeAcc1, thisNodeAcc2).map(d.nextBlockTime).min
      d.testTime.setTimeIfGreater(blockTs3)

      val block4Ts = Seq(thisNodeAcc1, thisNodeAcc2).map(d.nextBlockTime).min
      appenderScheduler.tickNext("appender-1")
      minerScheduler.tickNext("miner-1")
      appenderScheduler.tickNext("appender-2")

      log.debug("Forge first microblock")
      d.utxPool.putIfNew(TxHelpers.transfer(thisNodeAcc3))
      utxEvents.onNext(())
      minerScheduler.tickNext("miner-2")
      appenderScheduler.tickNext("appender-3")

      log.debug("Forge last microblock")
      d.testTime.setTimeIfGreater(block4Ts - 1) // To exclude the latest microblock, see min-micro-block-age
      Seq(thisNodeAcc1, thisNodeAcc2).map { kp =>
        d.utxPool.putIfNew(TxHelpers.transfer(kp, amount = d.blockchain.balance(kp.toAddress) - 1.waves))
      }
      utxEvents.onNext(())
      minerScheduler.tickNext("miner-3")
      appenderScheduler.tickNext("appender-4")

      log.debug("Trigger thisNode forging")
      d.testTime.setTimeIfGreater(block4Ts + 1)
      (4 to 5).foreach { i =>
        minerScheduler.tickNext(s"miner-$i")
        appenderScheduler.tickNext(s"appender-${i + 1}")
      }

      val messages = minerImpl.inMemoryLog.getMessages
      Seq(thisNodeAcc1, thisNodeAcc2, thisNodeAcc3).foreach { kp =>
        messages.find(_.contains(s"${kp.toAddress} is lower than required for generation")) shouldBe defined
      }
    }
  }
}
