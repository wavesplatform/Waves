package com.wavesplatform.network
import java.util.concurrent.CountDownLatch

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.diffs.ProduceError._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.settings.SynchronizationSettings.UtxSynchronizerSettings
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import com.wavesplatform.utils.Schedulers
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.util.HashedWheelTimer
import monix.execution.atomic.AtomicInt
import monix.reactive.Observable
import monix.reactive.subjects.ReplaySubject
import org.scalatest.concurrent.Eventually
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{BeforeAndAfterAll, EitherValues, FreeSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.concurrent.duration._

class UtxPoolSynchronizerSpec extends FreeSpec with Matchers with BeforeAndAfterAll with Eventually with ScalaCheckPropertyChecks with EitherValues {
  private[this] val timer     = new HashedWheelTimer
  private[this] val scheduler = Schedulers.timeBoundedFixedPool(timer, 1.second, 2, "test-utx-sync")

  "UtxPoolSynchronizer" - {
    def sleep(millis: Int)(tx: Transaction): TracedResult[ValidationError, Boolean] = {
      while (true) {}
      TracedResult(Right(true))
    }

    "rejects transactions which take too long to validate" in withUPS(sleep(Int.MaxValue), Observable.evalOnce(true)) { ups =>
      ups.publish(GenesisTransaction.create(PublicKey(Array.emptyByteArray), 10L, 0L).explicitGet()).resultE should produce("Timeout executing task")
    }

    val latch   = new CountDownLatch(5)
    val counter = AtomicInt(10)

    def countTransactions(tx: Transaction): TracedResult[ValidationError, Boolean] = {
      if (counter.getAndDecrement() > 5) {
        while (true) {}
      }
      latch.countDown()
      TracedResult(Right(true))
    }

    "accepts only those transactions from network which can be validated quickly" in withUPS(countTransactions, Observable.evalOnce(true)) { ups =>
      1 to 10 foreach { i =>
        ups.tryPublish(GenesisTransaction.create(PublicKey(Array.emptyByteArray), i * 10L, 0L).explicitGet(), new EmbeddedChannel)
      }
      latch.await()
      counter.get() shouldEqual 0
    }

    val readiness = ReplaySubject.createLimited[Boolean](1)

    "rejects transactions when blockchain is stale" in withUPS(_ => TracedResult(Right(true)), readiness) { ups =>
      implicit val patienceConfig: PatienceConfig = PatienceConfig(timeout = scaled(Span(3, Seconds)))

      val tx = GenesisTransaction.create(PublicKey(Array.emptyByteArray), 10L, 0L).explicitGet()

      ups.publish(tx).resultE shouldBe 'left

      readiness.onNext(false)
      ups.publish(tx).resultE shouldBe 'left

      readiness.onNext(true)
      eventually(ups.publish(tx).resultE shouldBe 'right)

      forAll { i: Boolean =>
        readiness.onNext(i)
        ups.publish(tx).resultE shouldBe 'right
      }
    }
  }

  private def withUPS(putIfNew: Transaction => TracedResult[ValidationError, Boolean], readiness: Observable[Boolean])(
      f: UtxPoolSynchronizer => Unit
  ): Unit = {
    val ups = new UtxPoolSynchronizerImpl(UtxSynchronizerSettings(1000, 2, 1000, true), putIfNew, { (_, _) =>
    }, Observable.empty, readiness)(scheduler)
    f(ups)
    ups.close()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    scheduler.shutdown()
    scheduler.shutdown()
    timer.stop()
  }
}
