package com.wavesplatform.lang

import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.{FunctionHeader, Serde}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, FreeSpec, Matchers}
import scodec.bits.ByteVector

class SerdeTest extends FreeSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  "roundtrip" - {
    "CONST_LONG" in roundTripTest(CONST_LONG(1))
    "CONST_BYTEVECTOR" in roundTripTest(CONST_BYTEVECTOR(ByteVector[Byte](1)))
    "CONST_STRING" in roundTripTest(CONST_STRING("foo"))

    "IF" in roundTripTest(IF(TRUE, CONST_LONG(0), CONST_LONG(1)))

    "BLOCK" in roundTripTest(
      BLOCK(
        let = LET("foo", TRUE),
        body = FALSE
      )
    )

    "REF" in roundTripTest(REF("foo"))
    "TRUE" in roundTripTest(TRUE)
    "FALSE" in roundTripTest(FALSE)

    "GETTER" in roundTripTest(GETTER(REF("foo"), "bar"))

    "FUNCTION_CALL" - {
      "native" in roundTripTest(
        FUNCTION_CALL(
          function = FunctionHeader.Native(1),
          args = List(TRUE)
        )
      )

      "user" in roundTripTest(
        FUNCTION_CALL(
          function = FunctionHeader.User("foo"),
          args = List(TRUE)
        )
      )

      "empty args" in roundTripTest(
        FUNCTION_CALL(
          function = FunctionHeader.User("foo"),
          args = List.empty
        )
      )
    }

    "general" in forAll(BOOLgen(10)) {
      case (untypedExpr, _) => roundTripTest(untypedExpr)
    }

    "stack safety" in {
      val bigSum = (1 to 10000).foldLeft[EXPR](CONST_LONG(0)) { (r, i) =>
        FUNCTION_CALL(
          function = PureContext.sumLong,
          args = List(r, CONST_LONG(i))
        )
      }

      val expr: EXPR = FUNCTION_CALL(
        function = PureContext.eq,
        args = List(CONST_LONG(1), bigSum)
      )

      Serde.serialize(expr).nonEmpty shouldBe true
    }
  }

  private def roundTripTest(untypedExpr: Expressions.EXPR): Assertion = {
    val typedExpr = CompilerV1(PureContext.compilerContext, untypedExpr).map(_._1).explicitGet()
    roundTripTest(typedExpr)
  }

  private def roundTripTest(typedExpr: EXPR): Assertion = {
    val encoded = Serde.serialize(typedExpr)
    encoded.nonEmpty shouldBe true

    val decoded = Serde.deserialize(encoded).explicitGet()
    withClue(s"encoded bytes: [${encoded.mkString(", ")}]") {
      decoded shouldEqual typedExpr
    }
  }
}
