package com.wavesplatform.state2.diffs.smart.scenarios

import com.wavesplatform.lang.FunctionHeader
import com.wavesplatform.lang.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.Terms._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.state2.diffs.smart._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock

class OnlyTransferIsAllowedTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("transfer is allowed but lease is not due to predicate") {
    import Typed._

    val onlySend: EXPR = BINARY_OP(
      BINARY_OP(
        BINARY_OP(GETTER(REF("tx", TYPEREF("Transaction")), "type", LONG), EQ_OP, CONST_LONG(4), BOOLEAN),
        OR_OP,
        BINARY_OP(GETTER(REF("tx", TYPEREF("Transaction")), "type", LONG), EQ_OP, CONST_LONG(11), BOOLEAN),
        BOOLEAN
      ),
      AND_OP,
      FUNCTION_CALL(
        FunctionHeader("sigVerify", List(FunctionHeaderType.BYTEVECTOR, FunctionHeaderType.BYTEVECTOR, FunctionHeaderType.BYTEVECTOR)),
        List(
          GETTER(REF("tx", TYPEREF("Transaction")), "bodyBytes", BYTEVECTOR),
          GETTER(REF("tx", TYPEREF("Transaction")), "proof0", BYTEVECTOR),
          GETTER(REF("tx", TYPEREF("Transaction")), "senderPk", BYTEVECTOR)
        ),
        BOOLEAN
      ),
      BOOLEAN
    )
    forAll(preconditionsTransferAndLease(onlySend)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }

}
