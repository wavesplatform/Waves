package com.wavesplatform.mining

import cats.syntax.option.*
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.settings.*
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
  private val thisNodeAcc  = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 0)
  private val otherNodeAcc = TxHelpers.defaultSigner

  private val baseSettings       = DomainPresets.TransactionStateSnapshot
  private val microBlockInterval = 5.seconds
  private val minMicroBlockAge   = 3.seconds
  private val defaultSettings = baseSettings.copy(
    minerSettings = baseSettings.minerSettings.copy(quorum = 0, microBlockInterval = microBlockInterval, minMicroBlockAge = minMicroBlockAge)
  )

  "Miner continues from the last micro block" in Using.Manager { manager =>
    val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    manager.acquire(channels)(using _.close())

    var miner: Miner = Miner.Disabled
    val time         = TestTime() // TODO: migrate to d.testTime
    withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(thisNodeAcc, otherNodeAcc),
      miner = x => miner.scheduleMining(x),
      time = time
    ) { d =>
      val minerScheduler    = TestScheduler()
      val appenderScheduler = TestScheduler()

      d.wallet.generateNewAccounts(1).map(_.toAddress)
      log.debug(s"thisNodeAcc=${thisNodeAcc.toAddress}, otherNodeAcc=${otherNodeAcc.toAddress}")

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
      appenderScheduler.tickNext("this-appender-1", failIfNoTasks = false)

      log.debug("Append microBlock1")
      time.advance(microBlockInterval)
      val microBlock1 = d.createMicroBlock(signer = otherNodeAcc.some)(TxHelpers.transfer(to = otherNodeAcc.toAddress))
      d.appendMicroBlock(microBlock1)
      appenderScheduler.tickNext("this-appender-2", failIfNoTasks = false)

      log.debug("Append microBlock2 (ref for next block)")
      time.advance(microBlockInterval)
      d.appendMicroBlock(d.createMicroBlock(signer = otherNodeAcc.some)(TxHelpers.transfer(to = otherNodeAcc.toAddress)))
      val refLiquidBlockId = d.lastBlockId

      log.debug("Trigger thisNode forging")
      time.advance(minMicroBlockAge)
      val waitExtra = d.nextBlockTime(thisNodeAcc) - time.getTimestamp()
      if (waitExtra > 0) time.advance(waitExtra.millis)

      minerScheduler.tickNext("this-miner-1")
      appenderScheduler.tickNext("this-appender-3")

      val lastBlock = d.blockchain.lastBlockHeader.value
      lastBlock.header.reference shouldBe refLiquidBlockId
    }
  }.get

  "Miner references a previous micro block" in Using.Manager { manager =>
    val channels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
    manager.acquire(channels)(using _.close())

    var miner: Miner = Miner.Disabled
    val time         = TestTime() // TODO: migrate to d.testTime
    withDomain(
      defaultSettings,
      AddrWithBalance.enoughBalances(thisNodeAcc, otherNodeAcc),
      miner = x => miner.scheduleMining(x),
      time = time
    ) { d =>
      val minerScheduler    = TestScheduler()
      val appenderScheduler = TestScheduler()

      d.wallet.generateNewAccounts(1).map(_.toAddress)
      log.debug(s"thisNodeAcc=${thisNodeAcc.toAddress}, otherNodeAcc=${otherNodeAcc.toAddress}")

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
      appenderScheduler.tickNext("this-appender-1", failIfNoTasks = false)
      def appendMicroBlock(): Unit =
        d.appendMicroBlock(d.createMicroBlock(signer = otherNodeAcc.some)(TxHelpers.transfer(to = otherNodeAcc.toAddress)))

      log.debug("Append microBlock1 (ref for next block)")
      time.advance(microBlockInterval)
      appendMicroBlock()
      val liquidBlock1Id = d.lastBlockId
      appenderScheduler.tickNext("this-appender-2", failIfNoTasks = false)

      log.debug("Append microBlock2")
      time.advance(microBlockInterval)
      appendMicroBlock()
      val liquidBlock2Id = d.lastBlockId

      log.debug("Trigger thisNode forging")
      time.advance(minMicroBlockAge / 2)
      val waitExtra = d.nextBlockTime(thisNodeAcc) - time.getTimestamp()
      val refLiquidBlockId =
        if (waitExtra <= 0) liquidBlock1Id
        else {
          time.advance(waitExtra.millis)
          appendMicroBlock()
          liquidBlock2Id
        }

      minerScheduler.tickNext("this-miner-1")
      appenderScheduler.tickNext("this-appender-3")

      val lastBlock = d.blockchain.lastBlockHeader.value
      lastBlock.header.reference shouldBe refLiquidBlockId
    }
  }.get
}
