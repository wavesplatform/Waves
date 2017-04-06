package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2.{EffectiveBalanceSnapshot, Portfolio, portfolioMonoid}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.TransactionGen

class AssetTransactionsDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  property("Issue+Reissue+Burn do not break waves invariant") {

  }

  property("Cannot reissue non-existing alias") {

  }

  property("Cannot reissue non-owned alias") {

  }

  property("Cannot reissue non-reissuable alias") {

  }

  property("Cannot burn non-existing alias") {

  }

  property("Cannot burn non-owned alias") {

  }

}