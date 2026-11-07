package com.wavesplatform.mining

import cats.syntax.option.*
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.test.{FreeSpec, TestSchedulerOps}
import com.wavesplatform.transaction.{Transaction, TxHelpers}
import com.wavesplatform.wallet.Wallet
import monix.execution.schedulers.TestScheduler
import monix.reactive.subjects.ConcurrentSubject
import org.scalatest.EitherValues
import org.scalatest.time.SpanSugar.convertLongToGrainOfTime

class MinerScheduleSuite extends FreeSpec with WithMiner with TestSchedulerOps with EitherValues {
  private val thisNodeAcc  = Wallet.generateNewAccount(Domain.DefaultWalletSeed, nonce = 0)
  private val otherNodeAcc = TxHelpers.defaultSigner

  private val microBlockInterval = 5.seconds
  private val minMicroBlockAge   = 3.seconds

  private val baseSettings = DomainPresets.TransactionStateSnapshot
  private val defaultSettings = baseSettings.copy(
    minerSettings = baseSettings.minerSettings.copy(quorum = 0, microBlockInterval = microBlockInterval, minMicroBlockAge = minMicroBlockAge)
  )

  "forges on top of a better liquid block that replaced our own" in withThisNodeMiner { (d, appenderScheduler, kickUtx) =>
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

    // No appender ticks until the better block: the clock passes our next block time here, so an attempt would forge on the worse block
    log.debug("Append microBlock")
    d.testTime.advance(microBlockInterval)
    d.appendMicroBlock(d.createMicroBlock(signer = thisNodeAcc.some)(mkTx()))

    log.debug("Append better liquid block")
    d.appender.appendBlockWithoutFallback(betterBlock) should beRight

    log.debug("Trigger thisNode forging")
    d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
    kickUtx()
    appenderScheduler.tickNext("this-appender-1")

    withClue(s"lastBlock.id=${d.lastBlockId}, reference: ") {
      val lbh = d.lastBlock.header
      lbh.reference shouldBe betterBlock.id()
      lbh.generator shouldBe thisNodeAcc.publicKey
    }
  }

  "forges on top of a key block appended without microblocks" in withThisNodeMiner { (d, appenderScheduler, _) =>
    log.debug("Trigger thisNode forging")
    val parentBlockId = d.lastBlockId

    d.testTime.setTimeIfGreater(d.nextBlockTime(thisNodeAcc))
    appenderScheduler.tickNext("this-appender-1")

    val lbh = d.lastBlock.header
    lbh.reference shouldBe parentBlockId
    lbh.generator shouldBe thisNodeAcc.publicKey
  }

  "forges on top of the last microblock of another miner's block" in withThisNodeMiner { (d, appenderScheduler, _) =>
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
    appenderScheduler.tickNext("this-appender-3")

    val lbh = d.lastBlock.header
    lbh.reference shouldBe parentBlockId
    lbh.generator shouldBe thisNodeAcc.publicKey
  }

  private def mkTx(): Transaction = TxHelpers.transfer(from = thisNodeAcc, to = otherNodeAcc.toAddress)

  /** Starts after block 2 forged by this node; `kickUtx` puts a transaction into the UTX pool and notifies the microblock miner */
  private def withThisNodeMiner(f: (Domain, TestScheduler, () => Unit) => Unit): Unit = {
    val minerScheduler    = TestScheduler()
    val appenderScheduler = TestScheduler()
    val utxEvents         = ConcurrentSubject.publish[Unit](using minerScheduler)

    withDomainAndTestMiner(defaultSettings, AddrWithBalance.enoughBalances(thisNodeAcc, otherNodeAcc), minerScheduler, appenderScheduler, utxEvents) {
      (d, _) =>
        d.appendBlock()
        appenderScheduler.tickNext("this-appender-0")

        f(
          d,
          appenderScheduler,
          () => {
            d.utxPool.putIfNew(mkTx())
            utxEvents.onNext(())
          }
        )
    }
  }
}
