package com.wavesplatform.mining

import com.wavesplatform.block.Block.ProtoBlockVersion
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.IntegerDataEntry
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.test.{PropSpec, TestTime}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxHelpers.*
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global
import org.scalatest.Assertion

import java.util.concurrent.CountDownLatch
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

class DiscardedMicroBlocksDiffTest extends PropSpec with WithDomain {
  property("interim waves balance") {
    testInterimState(
      transfer(amount = 123),
      _.accountsApi.balance(secondAddress) shouldBe 123
    )
  }

  property("interim asset balance") {
    val issueTx     = issue()
    val issuedAsset = IssuedAsset(issueTx.id())
    testInterimState(
      transfer(amount = 123, asset = issuedAsset),
      _.accountsApi.assetBalance(secondAddress, issuedAsset) shouldBe 123,
      preconditions = Seq(issueTx)
    )
  }

  property("interim account data") {
    testInterimState(
      dataEntry(defaultSigner, IntegerDataEntry("key", 1)),
      _.accountsApi.data(defaultAddress, "key").map(_.value) shouldBe Some(1)
    )
  }

  property("interim account script") {
    testInterimState(
      setScript(defaultSigner, TestCompiler(V6).compileExpression("true")),
      _.accountsApi.script(defaultAddress) should not be empty
    )
  }

  property("interim active leases") {
    testInterimState(
      lease(),
      _.accountsApi.activeLeases(defaultAddress).toListL.runSyncUnsafe() should not be empty
    )
  }

  private def testInterimState(tx: Transaction, assert: Domain => Assertion, preconditions: Seq[Transaction] = Nil): Assertion = {
    val waitInterimState = new CountDownLatch(1)
    val endInterimState  = new CountDownLatch(1)
    withDomain(
      RideV6,
      AddrWithBalance.enoughBalances(defaultSigner),
      beforeSetPriorityDiffs = { () =>
        waitInterimState.countDown()
        endInterimState.await()
      }
    ) { d =>
      val previousBlockId = d.appendBlock(preconditions: _*).id()
      d.appendMicroBlock(tx)
      assert(d)

      val keyBlock       = d.createBlock(ProtoBlockVersion, Nil, Some(previousBlockId))
      val appendKeyBlock = BlockAppender(d.blockchain, TestTime(), d.utxPool, d.posSelector, Scheduler.global, verify = false)(keyBlock).runToFuture

      waitInterimState.await()
      val check = Future(assert(d))
      endInterimState.countDown()

      Await.result(check, Inf)
      Await.result(appendKeyBlock, Inf)
      assert(d)
    }
  }
}
