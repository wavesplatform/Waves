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

  property("handles let expression correctly") {
    val Plus = FunctionHeader("+", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG)) ///refac?
    val expr =
      BLOCK(
        LET("a", CONST_LONG(1)),
        BLOCK(LET("b", CONST_LONG(2)), BLOCK(LET("c", FUNCTION_CALL(Plus, List(REF("a", LONG), REF("b", LONG)), LONG)), REF("c", LONG), LONG), LONG),
        LONG
      )
    val estimate = ScriptEstimator(Map(Plus -> 3L), expr)
    estimate shouldBe 'right
    estimate.right.get shouldBe 26
  }

  property("handles const expression correctly") {
    val estimate = ScriptEstimator(Map.empty, FALSE)
    estimate shouldBe 'right
    estimate.right.get shouldBe 1
  }
}
