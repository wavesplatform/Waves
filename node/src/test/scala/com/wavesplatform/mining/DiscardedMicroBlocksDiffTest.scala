package com.wavesplatform.mining

import com.wavesplatform.block.Block.ProtoBlockVersion
import com.wavesplatform.db.WithDomain
import com.wavesplatform.state.appender.BlockAppender
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.test.{PropSpec, TestTime}
import com.wavesplatform.transaction.TxHelpers.*
import monix.execution.Scheduler
import monix.execution.Scheduler.Implicits.global

import java.util.concurrent.CountDownLatch
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

class DiscardedMicroBlocksDiffTest extends PropSpec with WithDomain {
  property("interim balance") {
    val waitInterimState = new CountDownLatch(1)
    val exitInterimState = new CountDownLatch(1)
    withDomain(
      RideV6,
      beforeSetPriorityDiffs = { () =>
        waitInterimState.countDown()
        exitInterimState.await()
      }
    ) { d =>
      val appendBlock = BlockAppender(d.blockchain, TestTime(), d.utxPool, d.posSelector, Scheduler.global, verify = false) _

      val recipient     = secondAddress
      val startBalance  = d.balance(recipient)
      val transferTx    = transfer(amount = 123)
      def balanceDiff() = d.accountsApi.balance(recipient) - startBalance

      val previousBlockId = d.appendBlock().id()
      d.appendMicroBlock(transferTx)
      balanceDiff() shouldBe 123

      val keyBlock       = d.createBlock(ProtoBlockVersion, Nil, Some(previousBlockId))
      val appendKeyBlock = appendBlock(keyBlock).runToFuture

      waitInterimState.await()
      val check = Future(balanceDiff() shouldBe 123)
      exitInterimState.countDown()

      Await.result(check, Inf)
      Await.result(appendKeyBlock, Inf)
      balanceDiff() shouldBe 123
    }
  }
}
