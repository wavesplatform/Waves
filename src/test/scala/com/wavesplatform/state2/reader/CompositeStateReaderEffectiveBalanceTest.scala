package com.wavesplatform.state2.reader

import cats.kernel.Monoid
import com.wavesplatform.state2.{BlockDiff, Diff, EffectiveBalanceSnapshot}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}
import scorex.account.Account

class CompositeStateReaderEffectiveBalanceTest extends FreeSpec with MockFactory with Matchers {

  val acc: Account = Account.fromPublicKey(Array.emptyByteArray)
  val innerHeight = 1000

  "blockDiff contains no effective balance changes" in {
    val heightDiff = 100
    val blockDiff = BlockDiff(
      txsDiff = Monoid[Diff].empty,
      heightDiff = heightDiff,
      effectiveBalanceSnapshots = Seq.empty)

    val innerEffectiveBalance = 10000

    val inner = stub[StateReader]
    (inner.effectiveBalanceAtHeightWithConfirmations _).when(*, *, *).returns(innerEffectiveBalance)

    val composite = new CompositeStateReader(inner, blockDiff)
    composite.effectiveBalanceAtHeightWithConfirmations(acc, innerHeight + heightDiff, 30) shouldBe 10000
  }

  "blockDiff contains effective balance changes" - {
    "confirmations required is less than blockDiff height" - {
      "block diff contains records in the past in the requested range" in {
        val heightDiff = 100
        val blockDiff = BlockDiff(
          txsDiff = Monoid[Diff].empty,
          heightDiff = heightDiff,
          effectiveBalanceSnapshots = Seq(
            EffectiveBalanceSnapshot(acc, innerHeight + 80, 10000, 50000)))

        val inner = stub[StateReader]
        (inner.height _).when().returns(innerHeight)

        val composite = new CompositeStateReader(inner, blockDiff)
        composite.effectiveBalanceAtHeightWithConfirmations(acc, innerHeight + heightDiff, 30) shouldBe 10000
      }

      "block diff contains records but they are  out of requested range(too old)" in {
        val heightDiff = 100
        val blockDiff = BlockDiff(
          txsDiff = Monoid[Diff].empty,
          heightDiff = heightDiff,
          effectiveBalanceSnapshots = Seq(
            EffectiveBalanceSnapshot(acc, innerHeight + 80, 20000, 4000),
            EffectiveBalanceSnapshot(acc, innerHeight + 50, 50000, 20000),
            EffectiveBalanceSnapshot(acc, innerHeight + 90, 4000, 10000)))

        val inner = stub[StateReader]
        (inner.height _).when().returns(innerHeight)

        val composite = new CompositeStateReader(inner, blockDiff)
        composite.effectiveBalanceAtHeightWithConfirmations(acc, innerHeight + heightDiff, 2) shouldBe 10000
      }

    }

    "confirmations required is greater or equal blockdiff.height" - {

      "nothing is stored(Genesis block snapshot results are in memory)" in {
        val heightDiff = 100
        val blockDiff = BlockDiff(
          txsDiff = Monoid[Diff].empty,
          heightDiff = heightDiff,
          effectiveBalanceSnapshots = Seq(
            EffectiveBalanceSnapshot(acc, 80, 2000, 10000),
            EffectiveBalanceSnapshot(acc, 90, 10000, 50000)))

        val inner = stub[StateReader]
        (inner.height _).when().returns(0)

        val composite = new CompositeStateReader(inner, blockDiff)
        composite.effectiveBalanceAtHeightWithConfirmations(acc, heightDiff, heightDiff + 100) shouldBe 2000
      }

      "some history of acc is stored" in {
        val heightDiff = 100
        val blockDiff = BlockDiff(
          txsDiff = Monoid[Diff].empty,
          heightDiff = heightDiff,
          effectiveBalanceSnapshots = Seq(
            EffectiveBalanceSnapshot(acc, 80, 2000, 10000),
            EffectiveBalanceSnapshot(acc, 90, 10000, 50000)))

        val inner = stub[StateReader]
        (inner.height _).when().returns(innerHeight)
        val innerEffectiveBalance = 500
        (inner.effectiveBalanceAtHeightWithConfirmations _).when(*, *, *).returns(innerEffectiveBalance)

        val composite = new CompositeStateReader(inner, blockDiff)
        composite.effectiveBalanceAtHeightWithConfirmations(acc, heightDiff, heightDiff + 50) shouldBe 500
      }
    }
  }
}
