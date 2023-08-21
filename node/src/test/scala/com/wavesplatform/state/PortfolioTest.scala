package com.wavesplatform.state

import java.nio.charset.StandardCharsets
import com.wavesplatform.TestValues
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.test.FunSuite
import com.wavesplatform.transaction.Asset.IssuedAsset

import scala.collection.immutable.VectorMap

class PortfolioTest extends FunSuite {
  test("pessimistic - should return only withdraws") {
    val fooKey = IssuedAsset(ByteStr("foo".getBytes(StandardCharsets.UTF_8)))
    val barKey = IssuedAsset(ByteStr("bar".getBytes(StandardCharsets.UTF_8)))
    val bazKey = IssuedAsset(ByteStr("baz".getBytes(StandardCharsets.UTF_8)))

    val orig = Portfolio(
      balance = -10,
      lease = LeaseBalance(
        in = 11,
        out = 12
      ),
      assets = VectorMap(
        fooKey -> -13,
        barKey -> 14,
        bazKey -> 0
      )
    )

    val p = orig.pessimistic
    p.balance shouldBe orig.balance
    p.lease.in shouldBe 0
    p.lease.out shouldBe orig.lease.out
    p.assets(fooKey) shouldBe orig.assets(fooKey)
    p.assets shouldNot contain(barKey)
    p.assets shouldNot contain(bazKey)
  }

  test("pessimistic - positive balance is turned into zero") {
    val orig = Portfolio(
      balance = 10
    )

    val p = orig.pessimistic
    p.balance shouldBe 0
  }

  test("prevents overflow of Waves") {
    Portfolio(Long.MaxValue - 1L).combine(Portfolio(Long.MaxValue - 2L)) shouldBe Left("Waves balance sum overflow")
  }

  test("prevents overflow of assets") {
    val assetId = TestValues.asset
    val arg1    = Portfolio.build(assetId, Long.MaxValue - 1L)
    val arg2    = Portfolio.build(assetId, Long.MaxValue - 2L)
    arg1.combine(arg2) shouldBe Left(s"asset $assetId sum overflow")
  }

  test("prevents overflow of lease in balances") {
    val arg1 = Portfolio(0L, LeaseBalance(in = Long.MaxValue - 1L, out = 0))
    val arg2 = Portfolio(0L, LeaseBalance(in = Long.MaxValue - 1L, out = 0))
    arg1.combine(arg2) shouldBe Left("Lease in sum overflow")
  }

  test("prevents overflow of lease out balances") {
    val arg1 = Portfolio(0L, LeaseBalance(out = Long.MaxValue - 1L, in = 0))
    val arg2 = Portfolio(0L, LeaseBalance(out = Long.MaxValue - 1L, in = 0))
    arg1.combine(arg2) shouldBe Left("Lease out sum overflow")
  }

  test("prevents overflow of effective balance") {
    Portfolio(
      Long.MaxValue - 2L,
      LeaseBalance(in = Long.MaxValue - 1L, out = 0)
    ).effectiveBalance(false) shouldBe Left("Effective balance sum overflow")
  }
}
