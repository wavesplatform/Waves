package com.wavesplatform.state2.reader

import cats.kernel.Monoid
import com.wavesplatform.state2.{BlockDiff, Diff, EffectiveBalanceSnapshot}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FunSuite, Matchers}
import scorex.account.Account

class CompositeStateReaderTest extends FunSuite with MockFactory with Matchers {

  val acc: Account = Account.fromPublicKey(Array.emptyByteArray)
  val innerHeight = 1000

  test("exposes inner info" +
    " if blockDiff contains no related info") {
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


  test("exposes minimum of all 'current' and  one 'previous' of oldest record of diff" +
    " if confirmations required is less than blockDiff height and info is present") {
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


}
