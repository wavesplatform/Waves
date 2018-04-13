package com.wavesplatform.state2.diffs.smart.scenarios

import com.wavesplatform.lang.v1.{Parser, TypeChecker}
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2.diffs.smart._
import com.wavesplatform.utils.dummyTypeCheckerContext
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock

class OnlyTransferIsAllowedTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("transfer is allowed but lease is not due to predicate") {

    val scriptText =
      s"""
         |
         | if (tx.type == 4 || tx.type == 11)
         |  then sigVerify(tx.bodyBytes,tx.proof0,tx.senderPk)
         |  else false
         |
      """.stripMargin
    val untyped         = Parser(scriptText).get.value
    val transferAllowed = TypeChecker(dummyTypeCheckerContext, untyped).explicitGet()

    forAll(preconditionsTransferAndLease(transferAllowed)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }

}
