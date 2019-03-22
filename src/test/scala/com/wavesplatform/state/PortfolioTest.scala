package com.wavesplatform.state

import java.nio.charset.StandardCharsets

import cats._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction.Asset.IssuedAsset
import org.scalatest.{FunSuite, Matchers}

class PortfolioTest extends FunSuite with Matchers {
  test("pessimistic - should return only withdraws") {
    val Seq(fooKey, barKey, bazKey) = Seq("foo", "bar", "baz").map(x => IssuedAsset(ByteStr(x.getBytes(StandardCharsets.UTF_8))))

    val orig = Portfolio(
      balance = -10,
      lease = LeaseBalance(
        in = 11,
        out = 12
      ),
      assets = Map(
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
      balance = 10,
      lease = LeaseBalance(0, 0),
      assets = Map.empty
    )

    val p = orig.pessimistic
    p.balance shouldBe 0
  }

  test("prevents overflow of assets") {
    val assetId = IssuedAsset(ByteStr(Array.empty))
    val arg1    = Portfolio(0L, LeaseBalance.empty, Map(assetId -> (Long.MaxValue - 1L)))
    val arg2    = Portfolio(0L, LeaseBalance.empty, Map(assetId -> (Long.MaxValue - 2L)))
    Monoid.combine(arg1, arg2).assets(assetId) shouldBe Long.MinValue
  }
}
