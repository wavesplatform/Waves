package com.wavesplatform.mining

import cats.syntax.option.*
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.state
import com.wavesplatform.state.*
import com.wavesplatform.test.{FreeSpec, TestSchedulerOps, TestTime}
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.schedulers.TestScheduler
import monix.reactive.Observable
import org.scalatest.EitherValues
import org.scalatest.time.SpanSugar.convertLongToGrainOfTime

import scala.util.Using

class LastMicroBlockSuite extends FreeSpec with WithDomain with TestSchedulerOps with EitherValues {
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
      val block2 = d.createBlock(version = Block.ProtoBlockVersion, txs = Seq.empty, generator = thisNodeAcc1, strictTime = true)
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
      val block2 = d.createBlock(version = Block.ProtoBlockVersion, txs = Seq.empty, generator = otherNodeAcc, strictTime = true)
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
}
