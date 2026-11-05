package com.wavesplatform.network
import cats.kernel.Eq
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.db.WithDomain
import com.wavesplatform.settings.SynchronizationSettings.UtxSynchronizerSettings
import com.wavesplatform.test.DomainPresets.RideV6
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.TxHelpers.transfer
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utils.Schedulers
import monix.execution.atomic.AtomicInt
import monix.reactive.Observable

import scala.concurrent.Future
import scala.concurrent.duration.*

class TransactionSynchronizerSpec extends PropSpec with WithDomain {
  property("synchronizer should broadcast transactions on new both microblocks and blocks") {
    withDomain(RideV6) { d =>
      val blockIds =
        Observable
          .repeatEval(d.blockchain.lastBlockId.getOrElse(ByteStr.empty))
          .distinctUntilChanged(using Eq.fromUniversalEquals)

      val tx  = transfer()
      val txs = Observable.repeatEval(tx)

      val broadcastCount = AtomicInt(0)

      // Waits for the count to reach `expected`, then requires it to be exactly that. Waiting for `!= expected` instead
      // spun forever once the count overshot, hanging the whole unit test run in CI.
      def awaitBroadcasts(expected: Int): Unit = {
        val deadline = 10.seconds.fromNow
        while (broadcastCount.get() < expected && deadline.hasTimeLeft()) Thread.sleep(10)
        withClue(s"broadcasts after ${expected - 1} appends:") { broadcastCount.get() shouldBe expected }
      }

      val scheduler = Schedulers.fixedPool(4, "synchronizer")
      try {
        val synchronizer = TransactionSynchronizer(
          UtxSynchronizerSettings(1000000, 8, 5000, true),
          blockIds,
          txs.map((null, _)),
          (_, _) => Future.successful { broadcastCount.increment(); TracedResult(Right(true)) }
        )(using scheduler)

        try {
          awaitBroadcasts(1)
          (1 to 20).foreach { i =>
            if (i % 2 == 1)
              d.appendBlock()
            else
              d.appendMicroBlock(transfer())
            awaitBroadcasts(i + 1)
          }
        } finally synchronizer.cancel()
      } finally scheduler.shutdown()
    }
  }
}
