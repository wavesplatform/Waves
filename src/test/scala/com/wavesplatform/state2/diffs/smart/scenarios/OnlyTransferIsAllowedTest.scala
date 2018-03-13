package com.wavesplatform.state2.diffs.smart.scenarios

import com.wavesplatform.lang.Terms._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2.diffs.smart._
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock

class OnlyTransferIsAllowedTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  property("transfer is allowed but lease is not due to predicate") {
    import Typed._

    val onlySend: EXPR = BINARY_OP(
      BINARY_OP(
        BINARY_OP(GETTER(REF("TX", TYPEREF("Transaction")), "TYPE", LONG), EQ_OP, CONST_LONG(4), BOOLEAN),
        OR_OP,
        BINARY_OP(GETTER(REF("TX", TYPEREF("Transaction")), "TYPE", LONG), EQ_OP, CONST_LONG(11), BOOLEAN),
        BOOLEAN
      ),
      AND_OP,
      FUNCTION_CALL(
        "SIGVERIFY",
        List(
          GETTER(REF("TX", TYPEREF("Transaction")), "BODYBYTES", BYTEVECTOR),
          GETTER(REF("TX", TYPEREF("Transaction")), "PROOFA", BYTEVECTOR),
          GETTER(REF("TX", TYPEREF("Transaction")), "SENDERPK", BYTEVECTOR)
        ),
        BOOLEAN
      ),
      BOOLEAN
    )
    forAll(preconditionsTransferAndLease(onlySend)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(db, Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(db, Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }

}
