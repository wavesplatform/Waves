package com.wavesplatform.lang.v1

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ScriptEstimatorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  property("successful on very deep expressions(stack overflow check)") {
    val sumHeader = FunctionHeader("+", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG))
    val expr = (1 to 100000).foldLeft[EXPR](CONST_LONG(0)) { (acc, _) =>
      FUNCTION_CALL(sumHeader, List(CONST_LONG(1), acc), LONG)
    }

    ScriptEstimator(Map(sumHeader -> 1L), expr) shouldBe 'right
  }

  property("handles const expression correctly") {
    val estimate = ScriptEstimator(Map.empty, FALSE)
    estimate.right.get shouldBe 1
  }

  property("handles let expression correctly") {
    val Plus     = FunctionHeader("+", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG)) ///refac?
    val expr     = BLOCK(LET("t", TRUE), REF("t", BOOLEAN), BOOLEAN)
    val estimate = ScriptEstimator(Map(Plus -> 3L), expr)
    estimate.right.get shouldBe 8
  }

  property("evaluates let statement lazily") {
    val eager = BLOCK(LET("txtype", GETTER(REF("tx", CASETYPEREF("Transaction")), "type", LONG)), REF("txtype", LONG), BOOLEAN)
    ScriptEstimator(Map.empty, eager).right.get shouldBe 11

    val lzy = BLOCK(LET("txtype", GETTER(REF("tx", CASETYPEREF("Transaction")), "type", LONG)), TRUE, BOOLEAN) // does not reference `txtype`
    ScriptEstimator(Map.empty, lzy).right.get shouldBe 6
  }

  property("evaluates if statement lazily") {
    val GetElement = FunctionHeader("GetElement", List(FunctionHeaderType.LIST(FunctionHeaderType.BYTEVECTOR), FunctionHeaderType.LONG))
    val Gt         = FunctionHeader(">", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG))
    val Size       = FunctionHeader("size", List(FunctionHeaderType.BYTEVECTOR))
    val txRef      = REF("tx", CASETYPEREF("Transaction"))

    val script = BLOCK(
      LET("a", FUNCTION_CALL(GetElement, List(GETTER(txRef, "proofs", LIST(BYTEVECTOR)), CONST_LONG(0)), BYTEVECTOR)),
      BLOCK(
        LET("b", FUNCTION_CALL(GetElement, List(GETTER(txRef, "proofs", LIST(BYTEVECTOR)), CONST_LONG(1)), BYTEVECTOR)),
        BLOCK(
          LET("c",
              IF(FUNCTION_CALL(Gt, List(GETTER(txRef, "type", LONG), CONST_LONG(5)), BOOLEAN),
                 REF("a", BYTEVECTOR),
                 REF("b", BYTEVECTOR),
                 BYTEVECTOR)),
          FUNCTION_CALL(Gt, List(FUNCTION_CALL(Size, List(REF("c", BYTEVECTOR)), LONG), CONST_LONG(0)), BOOLEAN),
          BOOLEAN
        ),
        BOOLEAN
      ),
      BOOLEAN
    )
    val estimate = ScriptEstimator(Map(GetElement -> 10L, Gt -> 10L, Size -> 10L), script)
    estimate.right.get shouldBe 71
  }

  property("///") {
    val Plus  = FunctionHeader("+", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG))
    val Gt    = FunctionHeader(">", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG))
    val txRef = REF("tx", CASETYPEREF("Transaction"))

    val script = BLOCK(
      LET("a", GETTER(txRef, "type", LONG)),
      BLOCK(
        LET("b", GETTER(txRef, "amount", LONG)),
        BLOCK(LET("c", FUNCTION_CALL(Plus, List(REF("a", LONG), REF("b", LONG)), LONG)),
              FUNCTION_CALL(Gt, List(REF("b", LONG), CONST_LONG(0)), BOOLEAN),
              BOOLEAN),
        BOOLEAN
      ),
      BOOLEAN
    )
    val estimate = ScriptEstimator(Map(Plus -> 10L, Gt -> 10L), script)
    estimate.right.get shouldBe 32
  }
}
