package com.wavesplatform.lang.v1

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ScriptEstimatorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {
  val Plus  = FunctionHeader("+", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG))
  val Minus = FunctionHeader("-", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG))
  val Gt    = FunctionHeader(">", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG))

  val FunctionCosts: Map[FunctionHeader, Long] = Map(Plus -> 100, Minus -> 10, Gt -> 10)

  property("successful on very deep expressions(stack overflow check)") {
    val expr = (1 to 100000).foldLeft[EXPR](CONST_LONG(0)) { (acc, _) =>
      FUNCTION_CALL(Plus, List(CONST_LONG(1), acc), LONG)
    }

    ScriptEstimator(FunctionCosts, expr) shouldBe 'right
  }

  property("handles const expression correctly") {
    ScriptEstimator(Map.empty, FALSE).right.get shouldBe 1
  }

  property("handles getter expression correctly") {
    // tx.type
    val expr = GETTER(REF("tx", CASETYPEREF("Transaction")), "type", LONG)
    ScriptEstimator(Map.empty, expr).right.get shouldBe 2 + 2
  }

  property("evaluates let statement lazily") {
    // let t = 1 + 1
    // t
    val eager = BLOCK(LET("x", FUNCTION_CALL(Plus, List(CONST_LONG(1), CONST_LONG(1)), LONG)), REF("x", LONG), LONG)
    ScriptEstimator(FunctionCosts, eager).right.get shouldBe 109

    // let t = 1 + 1  // unused
    // 2
    val lzy = BLOCK(LET("x", FUNCTION_CALL(Plus, List(CONST_LONG(1), CONST_LONG(1)), LONG)), CONST_LONG(2), LONG)
    ScriptEstimator(FunctionCosts, lzy).right.get shouldBe 6

    // let x = 2 + 2  // evaluated once only
    // let y = x - x
    // x - y
    val onceOnly = BLOCK(
      LET("x", FUNCTION_CALL(Plus, List(CONST_LONG(2), CONST_LONG(2)), LONG)),
      BLOCK(LET("y", FUNCTION_CALL(Minus, List(REF("x", LONG), REF("x", LONG)), LONG)),
            FUNCTION_CALL(Minus, List(REF("x", LONG), REF("y", LONG)), LONG),
            LONG),
      LONG
    )
    ScriptEstimator(FunctionCosts, onceOnly).right.get shouldBe 140
  }

  property("ignores unused let statements") {
    // let a = 1 + 2  // unused
    // let b = 2
    // let c = a + b  // unused
    // b
    val script = BLOCK(
      LET("a", FUNCTION_CALL(Plus, List(CONST_LONG(1), CONST_LONG(2)), LONG)),
      BLOCK(
        LET("b", CONST_LONG(2)),
        BLOCK(
          LET("c", FUNCTION_CALL(Plus, List(REF("a", LONG), REF("b", LONG)), LONG)),
          REF("b", LONG),
          LONG
        ),
        LONG
      ),
      LONG
    )
    ScriptEstimator(FunctionCosts, script).right.get shouldBe 18
  }

  property("recursive let statement") {
    // let v = v; v
    val expr = BLOCK(LET("v", REF("v", LONG)), REF("v", LONG), LONG)
    ScriptEstimator(Map.empty, expr) shouldBe 'right
  }

  property("evaluates if statement lazily") {
    // let a = 1 + 2
    // let b = 3 + 4
    // let c = if tx.type > 5 then a else b
    // c
    val script = BLOCK(
      LET("a", FUNCTION_CALL(Plus, List(CONST_LONG(1), CONST_LONG(2)), LONG)),
      BLOCK(
        LET("b", FUNCTION_CALL(Plus, List(CONST_LONG(3), CONST_LONG(4)), LONG)),
        BLOCK(
          LET("c",
              IF(FUNCTION_CALL(Gt, List(GETTER(REF("tx", CASETYPEREF("Transaction")), "type", LONG), CONST_LONG(5)), BOOLEAN),
                 REF("a", LONG),
                 REF("b", LONG),
                 LONG)),
          REF("c", LONG),
          LONG
        ),
        LONG
      ),
      LONG
    )
    ScriptEstimator(FunctionCosts, script).right.get shouldBe 137
  }
}
