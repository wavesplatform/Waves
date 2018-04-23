package com.wavesplatform.lang.v1

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.Terms._
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ScriptComplexityCalculatorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  property("successful on very deep expressions(stack overflow check)") {
    val sumHeader = FunctionHeader("+", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG))
    val expr = (1 to 100000).foldLeft[Typed.EXPR](Typed.CONST_LONG(0)) { (acc, _) =>
      Typed.FUNCTION_CALL(sumHeader, List(Typed.CONST_LONG(1), acc), Terms.LONG)
    }

    ScriptComplexityCalculator(expr, Map(sumHeader -> 1L)) shouldBe 'right
  }

}
