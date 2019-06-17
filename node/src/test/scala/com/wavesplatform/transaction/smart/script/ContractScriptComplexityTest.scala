package com.wavesplatform.transaction.smart.script

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.contract.DApp._
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.ContractScript
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext._
import com.wavesplatform.lang.v1.testing.TypedScriptGen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ContractScriptComplexityTest extends PropSpec with PropertyChecks with Matchers with TypedScriptGen {

  property("estimate contract script correctly") {
    val contract = DApp(
      ByteStr.empty,
      List.empty,
      List(
        CallableFunction(
          CallableAnnotation(""),
          Terms.FUNC(
            "first",
            List("arg1", "arg2"),
            LET_BLOCK(
              LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
              REF("x")
            )
          )
        ),
        CallableFunction(
          CallableAnnotation(""),
          Terms.FUNC(
            "default",
            List(),
            LET_BLOCK(
              LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
              REF("x")
            )
          )
        )
      ),
      Some(
        VerifierFunction(
          VerifierAnnotation(""),
          Terms.FUNC(
            "third",
            List("arg1", "arg2"),
            LET_BLOCK(
              LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
              LET_BLOCK(
                LET("y", FUNCTION_CALL(sumLong.header, List(REF("x"), CONST_LONG(1)))),
                REF("y")
              )
            )
          )
        )
      )
    )

    ContractScript.estimateComplexity(V3, contract) shouldBe Right((41, Vector(("first", 32), ("default", 20), ("third", 41))))
  }

  property("estimate contract script with context correctly") {
    val contract = DApp(
      ByteStr.empty,
      List(
        LET("y", FUNCTION_CALL(sumString.header, List(CONST_STRING("a"), CONST_STRING("b")))),
        LET("z", FUNCTION_CALL(sumString.header, List(CONST_STRING("c"), CONST_STRING("d"))))
      ),
      List(
        CallableFunction(
          CallableAnnotation(""),
          Terms.FUNC(
            "first",
            List("arg1", "arg2"),
            LET_BLOCK(
              LET("x", FUNCTION_CALL(sumLong.header, List(REF("y"), REF("z")))),
              REF("x")
            )
          )
        ),
        CallableFunction(
          CallableAnnotation(""),
          Terms.FUNC(
            "default",
            List(),
            LET_BLOCK(
              LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
              REF("x")
            )
          )
        )
      ),
      Some(
        VerifierFunction(
          VerifierAnnotation(""),
          Terms.FUNC(
            "third",
            List("arg1", "arg2"),
            LET_BLOCK(
              LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
              LET_BLOCK(
                LET("y", FUNCTION_CALL(sumLong.header, List(REF("x"), CONST_LONG(1)))),
                REF("y")
              )
            )
          )
        )
      )
    )

    ContractScript.estimateComplexity(V3, contract) shouldBe Right((68, Vector(("first", 68), ("default", 30), ("third", 51))))
  }

  property("estimate contract script with context correctly 2") {
    val contract = DApp(
      ByteStr.empty,
      List(
        LET("y", FUNCTION_CALL(sumString.header, List(CONST_STRING("a"), CONST_STRING("b")))),
        LET("z", FUNCTION_CALL(sumString.header, List(CONST_STRING("c"), CONST_STRING("d"))))
      ),
      List(
        CallableFunction(
          CallableAnnotation(""),
          Terms.FUNC(
            "first",
            List("arg1", "arg2"),
            LET_BLOCK(
              LET("x", FUNCTION_CALL(sumLong.header, List(REF("y"), REF("z")))),
              REF("x")
            )
          )
        ),
        CallableFunction(
          CallableAnnotation(""),
          Terms.FUNC(
            "default",
            List(),
            LET_BLOCK(
              LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
              REF("x")
            )
          )
        )
      ),
      Some(
        VerifierFunction(
          VerifierAnnotation(""),
          Terms.FUNC(
            "second",
            List("arg1", "arg2"),
            LET_BLOCK(
              LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(0)))),
              LET_BLOCK(
                LET("y", FUNCTION_CALL(sumLong.header, List(REF("x"), CONST_LONG(1)))),
                REF("y")
              )
            )
          )
        )
      )
    )

    ContractScript.estimateComplexity(V3, contract) shouldBe Right((68, Vector(("first", 68), ("default", 30), ("second", 51))))
  }
}
