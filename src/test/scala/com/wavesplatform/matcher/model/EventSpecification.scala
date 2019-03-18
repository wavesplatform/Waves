package com.wavesplatform.matcher.model

import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.matcher.MatcherTestData
import com.wavesplatform.matcher.model.Events.OrderExecuted
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.exchange.AssetPair
import org.scalatest.{FreeSpec, Matchers}

class EventSpecification extends FreeSpec with Matchers with MatcherTestData {
  "Proper rounding scenario 1" in {
    val pair      = AssetPair(Waves, mkAssetId("BTC"))
    val counter   = sell(pair, 840340L, 0.00000238, matcherFee = Some(300000L))
    val submitted = buy(pair, 425532L, 0.00000238, matcherFee = Some(300000L))
    val exec      = OrderExecuted(LimitOrder(submitted), LimitOrder(counter), 0L)
    exec.executedAmount shouldBe 420169L
    exec.counterRemainingAmount shouldBe 420171L
    exec.counterRemainingAmount shouldBe counter.amount - exec.executedAmount

    exec.counterRemainingFee shouldBe 150001L

    exec.submittedRemainingAmount shouldBe 5363L
    exec.submittedRemainingAmount shouldBe submitted.amount - exec.executedAmount

    exec.submittedRemainingFee shouldBe 3781L
  }

  "Remaining fee and amount checks" in {
    val pair      = AssetPair(Waves, mkAssetId("BTC"))
    val counter   = sell(pair, 100000000, 0.0008, matcherFee = Some(2000L))
    val submitted = buy(pair, 120000000, 0.00085, matcherFee = Some(1000L))

    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter), 0L)
    exec.submittedRemainingAmount shouldBe 20000000L
    exec.submittedRemainingFee shouldBe 167L
  }

  "Reserved balance should empty after full rounded execution" in {
    val pair = AssetPair(mkAssetId("BTC"), mkAssetId("ETH"))

    val alicePk   = PrivateKeyAccount("alice".getBytes("utf-8"))
    val counter   = buy(pair, 923431000L, 0.00031887, matcherFee = Some(300000), sender = Some(alicePk))
    val bobPk     = PrivateKeyAccount("bob".getBytes("utf-8"))
    val submitted = sell(pair, 223345000L, 0.00031887, matcherFee = Some(300000), sender = Some(bobPk))

    val exec = OrderExecuted(LimitOrder(submitted), LimitOrder(counter), 0L)
    exec.executedAmount shouldBe 223344937L
  }
}
