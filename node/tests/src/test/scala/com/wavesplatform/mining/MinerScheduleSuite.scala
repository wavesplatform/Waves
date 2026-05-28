package com.wavesplatform.mining

import cats.syntax.option.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.state.*
import com.wavesplatform.test.{FreeSpec, TestSchedulerOps, WithResourceManager}
import com.wavesplatform.transaction.{Transaction, TxHelpers}
import com.wavesplatform.wallet.Wallet
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.schedulers.TestScheduler
import monix.reactive.subjects.ConcurrentSubject
import org.scalatest.EitherValues
import org.scalatest.time.SpanSugar.convertLongToGrainOfTime

import scala.util.Using

class MinerScheduleSuite extends FreeSpec with WithDomain with TestSchedulerOps with WithResourceManager with EitherValues {
  private val thisNodeAcc  = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 0)
  private val otherNodeAcc = TxHelpers.defaultSigner

  private val microBlockInterval = 5.seconds
  private val minMicroBlockAge   = 3.seconds

  "After new block (no NG state)" in pending // Hard to do, because, we haven't yet emulated restarts in such tests

  "After better liquid block" in new BaseTest {
    override def continue(d: Domain): Unit = {
      log.debug("Prepare worse and better blocks")
      val commonBlockId = d.lastBlockId
      val worseBlock = d.createBlock(
        generator = thisNodeAcc,
        ref = commonBlockId.some,
        strictTime = true,
        timestamp = Some(d.nextBlockTime(otherNodeAcc) + 1L)
      )
      val betterBlock = d.createBlock(generator = otherNodeAcc, ref = commonBlockId.some, strictTime = true)

      log.debug("Append worse block")
      d.appender.appendBlockWithoutFallback(worseBlock) should beRight
      appenderScheduler.tickNext("this-appender-1")

      log.debug("Append microBlock")
      d.testTime.advance(microBlockInterval)
      val microBlock = d.createMicroBlock(signer = thisNodeAcc.some)(mkTx())
      d.appendMicroBlock(microBlock)
      appenderScheduler.tickNext("this-appender-2")

      log.debug("Append better liquid block")
      d.appender.appendBlockWithoutFallback(betterBlock) should beRight

      log.debug("Trigger thisNode forging")
      d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
      d.kickUtx()
      appenderScheduler.tickNext("this-appender-3")
      minerScheduler.tickNext("this-miner-1")
      appenderScheduler.tickNext("this-appender-4")

      withClue(s"lastBlock.id=${d.lastBlockId}, reference: ") {
        val lbh = d.lastBlock.header
        lbh.reference shouldBe betterBlock.id()
        lbh.generator shouldBe thisNodeAcc.publicKey
      }
    }
  }.run()

  "After key block" in new BaseTest {
    override def continue(d: Domain): Unit = {
      log.debug("Trigger thisNode forging")
      val parentBlockId = d.lastBlockId

      d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
      minerScheduler.tickNext("this-miner-1")
      appenderScheduler.tickNext("this-appender-1")

      val lbh = d.lastBlock.header
      lbh.reference shouldBe parentBlockId
      lbh.generator shouldBe thisNodeAcc.publicKey
    }
  }.run()

  "After micro block" in new BaseTest {
    override def continue(d: Domain): Unit = {
      log.debug("Append block")
      d.appender.appendBlockWithoutFallback(d.createBlock(generator = otherNodeAcc, strictTime = true)) should beRight
      appenderScheduler.tickNext("this-appender-1")

      log.debug("Append micro block")
      d.testTime.advance(microBlockInterval)
      d.appendMicroBlock(d.createMicroBlock(signer = otherNodeAcc.some)(mkTx()))
      appenderScheduler.tickNext("this-appender-2")
      val parentBlockId = d.lastBlockId

      log.debug("Trigger thisNode forging")
      d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
      minerScheduler.tickNext("this-miner-1")
      appenderScheduler.tickNext("this-appender-3")

      val lbh = d.lastBlock.header
      lbh.reference shouldBe parentBlockId
      lbh.generator shouldBe thisNodeAcc.publicKey
    }
  }.run()

  private trait BaseTest {
    def baseSettings = DomainPresets.TransactionStateSnapshot
    def defaultSettings = baseSettings.copy(
      minerSettings = baseSettings.minerSettings.copy(quorum = 0, microBlockInterval = microBlockInterval, minMicroBlockAge = minMicroBlockAge)
    )

    val minerScheduler    = TestScheduler()
    val appenderScheduler = TestScheduler()
    val utxEvents         = ConcurrentSubject.publish[Unit](using minerScheduler)

    def continue(d: Domain): Unit

    def run(): Unit = Using.Manager { manager =>
      val channels = manager(new DefaultChannelGroup(GlobalEventExecutor.INSTANCE))(using _.close())

      var miner = Miner.StrictDisabledMiner
      withDomain(
        defaultSettings,
        AddrWithBalance.enoughBalances(thisNodeAcc, otherNodeAcc),
        miner = Miner.forwardTo(miner)
      ) { d =>
        d.wallet.generateNewAccounts(1)

        miner = new MinerImpl(
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

        d.appendBlock()
        appenderScheduler.tickNext("this-appender-0")

        continue(d)
      }
    }.get

    def mkTx(): Transaction = TxHelpers.transfer(from = thisNodeAcc, to = otherNodeAcc.toAddress)

    extension (d: Domain) {
      def kickUtx(): Unit = {
        d.utxPool.putIfNew(mkTx())
        utxEvents.onNext(())
      }
    }
  }
}
