package com.wavesplatform.state2.diffs.smart.scenarios

import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import com.wavesplatform.state2.diffs.{assertDiffAndState, assertDiffEi, produce}
import com.wavesplatform.state2.diffs.smart._
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import scorex.lagonaki.mocks.TestBlock

class LazyFieldAccessTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  private val goodScript =
    """
      |
      | if (TX.TYPE == 4) then (TX.ASSETID == None) else false
      |
      """.stripMargin

  private val badScript =
    """
      |
      | TX.ASSETID == None
      |
      """.stripMargin

  property("accessing field of transaction without checking its type first results on exception") {

    forAll(preconditionsTransferAndLease(goodScript)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(db, Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), fs) { case _ => () }
        assertDiffEi(db, Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), fs)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }

    forAll(preconditionsTransferAndLease(badScript)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(db, Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), fs) { case _ => () }
        assertDiffEi(db, Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), fs)(totalDiffEi =>
          totalDiffEi should produce("doesn't contain asset id"))
    }
  }
}
