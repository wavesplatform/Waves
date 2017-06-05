package com.wavesplatform.state2

import cats._
import org.scalatest.{FunSuite, Matchers}

class PortfolioTest extends FunSuite with Matchers {
  test("prevents overflow of assets") {
    val assetId = ByteStr(Array.empty)
    val arg1 = Portfolio(0L, LeaseInfo.empty, Map(assetId -> (Long.MaxValue - 1L)))
    val arg2 = Portfolio(0L, LeaseInfo.empty, Map(assetId -> (Long.MaxValue - 2L)))
    Monoid.combine(arg1, arg2).assets(assetId) shouldBe Long.MinValue
  }
}
