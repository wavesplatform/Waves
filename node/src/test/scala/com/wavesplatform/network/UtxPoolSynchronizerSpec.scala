package com.wavesplatform.network
import java.util.concurrent.CountDownLatch

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.SynchronizationSettings.UtxSynchronizerSettings
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.utils.Schedulers
import io.netty.util.HashedWheelTimer
import monix.execution.atomic.AtomicInt
import monix.reactive.Observable
import org.scalatest.{BeforeAndAfterAll, FreeSpec, Matchers}

import scala.concurrent.duration._

class UtxPoolSynchronizerSpec extends FreeSpec with Matchers with BeforeAndAfterAll {
  private[this] val timer     = new HashedWheelTimer
  private[this] val scheduler = Schedulers.timeBoundedFixedPool(timer, 1.second, 2, "test-utx-sync")

  "UtxPoolSynchronizer" - {
    val latch   = new CountDownLatch(5)
    val counter = AtomicInt(10)

    def countTransactions(tx: Transaction): TracedResult[ValidationError, Boolean] = {
      if (counter.getAndDecrement() > 5)
        while (!Thread.currentThread().isInterrupted) {} else
        latch.countDown()

      TracedResult(Right(true))
    }

    "accepts only those transactions from network which can be validated quickly" in withUPS(countTransactions) { ups =>
      1 to 10 foreach { i =>
        ups.publish(GenesisTransaction.create(PublicKey(new Array[Byte](32)).toAddress, i * 10L, 0L).explicitGet())
      }
      latch.await()
      counter.get() shouldEqual 0
    }
  }

  private def withUPS(putIfNew: Transaction => TracedResult[ValidationError, Boolean])(f: UtxPoolSynchronizer => Unit): Unit = {
    val ups =
      new UtxPoolSynchronizerImpl(UtxSynchronizerSettings(1000, 2, 1000, true), (tx, _) => putIfNew(tx), (_, _) => (), Observable.empty, scheduler)
    f(ups)
    ups.close()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    scheduler.shutdown()
    timer.stop()
  }
}
