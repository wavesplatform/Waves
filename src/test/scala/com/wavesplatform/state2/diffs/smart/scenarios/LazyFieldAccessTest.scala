package com.wavesplatform.state2.diffs.smart.scenarios

import com.wavesplatform.state2.diffs.smart._
import com.wavesplatform.state2.diffs.{assertDiffAndState, assertDiffEi, produce}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock

class LazyFieldAccessTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private val goodScript =
    """
      |
      | if (tx.type == 4) then (tx.assetId == None) else false
      |
      """.stripMargin

  private val badScript =
    """
      |
      | tx.assetId == None
      |
      """.stripMargin

  property("accessing field of transaction without checking its type first results on exception") {

    forAll(preconditionsTransferAndLease(goodScript)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }

    forAll(preconditionsTransferAndLease(badScript)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("doesn't contain asset id"))
    }
  }
}
