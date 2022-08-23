package com.wavesplatform.mining

import com.wavesplatform.block.Block.ProtoBlockVersion
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.IntegerDataEntry
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.test.{PropSpec, TestTime}
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxHelpers.*
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global

import java.util.concurrent.CountDownLatch
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

class DiscardedMicroBlocksDiffTest extends PropSpec with WithDomain {
  property("consistent interim balance") {
    Seq(true, false).foreach { useAsset =>
      val waitInterimState = new CountDownLatch(1)
      val endInterimState  = new CountDownLatch(1)
      withDomain(
        RideV6,
        beforeSetPriorityDiffs = { () =>
          waitInterimState.countDown()
          endInterimState.await()
        }
      ) { d =>
        val appendBlock = BlockAppender(d.blockchain, TestTime(), d.utxPool, d.posSelector, Scheduler.global, verify = false) _

        val recipient    = secondAddress
        val issueTx      = issue()
        val issuedAsset  = IssuedAsset(issueTx.id())
        val asset        = if (useAsset) issuedAsset else Waves
        val startBalance = d.balance(recipient, asset)
        val transferTx   = transfer(amount = 123, asset = asset)
        def balanceDiff() =
          if (useAsset)
            d.accountsApi.assetBalance(recipient, issuedAsset) - startBalance
          else
            d.accountsApi.balance(recipient) - startBalance

        val previousBlockId = d.appendBlock(issueTx).id()
        d.appendMicroBlock(transferTx)
        balanceDiff() shouldBe 123

        val keyBlock       = d.createBlock(ProtoBlockVersion, Nil, Some(previousBlockId))
        val appendKeyBlock = appendBlock(keyBlock).runToFuture

        waitInterimState.await()
        val check = Future(balanceDiff() shouldBe 123)
        endInterimState.countDown()

        Await.result(check, Inf)
        Await.result(appendKeyBlock, Inf)
        balanceDiff() shouldBe 123
      }
    }
  }

  property("consistent interim account data") {
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
      val appendBlock = BlockAppender(d.blockchain, TestTime(), d.utxPool, d.posSelector, Scheduler.global, verify = false) _

      val dataTx = dataEntry(defaultSigner, IntegerDataEntry("key", 1))
      def data() = d.accountsApi.data(defaultAddress, "key").map(_.value)

      val previousBlockId = d.appendBlock().id()
      d.appendMicroBlock(dataTx)
      data() shouldBe Some(1)

      val keyBlock       = d.createBlock(ProtoBlockVersion, Nil, Some(previousBlockId))
      val appendKeyBlock = appendBlock(keyBlock).runToFuture

      waitInterimState.await()
      val check = Future(data() shouldBe Some(1))
      endInterimState.countDown()

      Await.result(check, Inf)
      Await.result(appendKeyBlock, Inf)
      data() shouldBe Some(1)
    }
  }

  property("consistent interim account script") {
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
      val appendBlock = BlockAppender(d.blockchain, TestTime(), d.utxPool, d.posSelector, Scheduler.global, verify = false) _

      val setScriptTx = setScript(defaultSigner, TestCompiler(V6).compileExpression("true"))
      def hasScript() = d.accountsApi.script(defaultAddress).nonEmpty

      val previousBlockId = d.appendBlock().id()
      d.appendMicroBlock(setScriptTx)
      hasScript() shouldBe true

      val keyBlock       = d.createBlock(ProtoBlockVersion, Nil, Some(previousBlockId))
      val appendKeyBlock = appendBlock(keyBlock).runToFuture

      waitInterimState.await()
      val check = Future(hasScript() shouldBe true)
      endInterimState.countDown()

      Await.result(check, Inf)
      Await.result(appendKeyBlock, Inf)
      hasScript() shouldBe true
    }
  }
}
