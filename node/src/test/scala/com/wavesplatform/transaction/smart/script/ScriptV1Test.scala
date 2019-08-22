package com.wavesplatform.transaction.smart.script

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.testing.TypedScriptGen
import com.wavesplatform.state.diffs._
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ScriptV1Test extends PropSpec with PropertyChecks with Matchers with TypedScriptGen {

  property("ScriptV1.apply should permit BOOLEAN scripts") {
    forAll(BOOLEANgen(10)) { expr =>
      ExprScript(expr) shouldBe 'right
    }
  }

  property("Script.estimate should deny too complex scripts") {
    val byteStr = CONST_BYTESTR(ByteStr.fromBytes(1)).explicitGet()
    val expr = (1 to 21)
      .map { _ =>
        FUNCTION_CALL(
          function = FunctionHeader.Native(SIGVERIFY),
          args = List(byteStr, byteStr, byteStr)
        )
      }
      .reduceLeft[EXPR](IF(_, _, FALSE))

    Script.estimate(ExprScript(expr).explicitGet(), ScriptEstimatorV2) should produce("Script is too complex")
  }

  property("ScriptV1.apply should deny too big scripts") {
    val bigSum = (1 to 100).foldLeft[EXPR](CONST_LONG(0)) { (r, i) =>
      FUNCTION_CALL(
        function = FunctionHeader.Native(SUM_LONG),
        args = List(r, CONST_LONG(i))
      )
    }
    val expr = (1 to 9).foldLeft[EXPR](CONST_LONG(0)) { (r, i) =>
      FUNCTION_CALL(
        function = PureContext.eq.header,
        args = List(r, bigSum)
      )
    }

    ExprScript(expr) should produce("Script is too large")
  }

  property("19 sigVerify should fit in maxSizeInBytes") {
    val byteStr = CONST_BYTESTR(ByteStr.fromBytes(1)).explicitGet()
    val expr = (1 to 19)
      .map { _ =>
        FUNCTION_CALL(
          function = FunctionHeader.Native(SIGVERIFY),
          args = List(byteStr, byteStr, byteStr)
        )
      }
      .reduceLeft[EXPR](IF(_, _, FALSE))

    ExprScript(expr) shouldBe 'right
  }

  property("Expression block version check - successful on very deep expressions(stack overflow check)") {
    val expr = (1 to 100000).foldLeft[EXPR](CONST_LONG(0)) { (acc, _) =>
      FUNCTION_CALL(FunctionHeader.Native(SUM_LONG), List(CONST_LONG(1), acc))
    }

    com.wavesplatform.lang.v1.compiler.сontainsBlockV2(expr) shouldBe false
  }

}
