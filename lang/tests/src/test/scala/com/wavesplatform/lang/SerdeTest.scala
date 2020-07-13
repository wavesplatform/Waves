package com.wavesplatform.lang

import java.nio.charset.StandardCharsets

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.Expressions
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.{FunctionHeader, Serde}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Assertion, FreeSpec, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

import scala.util.Try

class SerdeTest extends FreeSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  private val caseObj = CaseObj(
    CASETYPEREF("Object type", Nil),
    Map(
      "field1" -> CONST_BYTESTR(ByteStr.fromBytes(1, 2, 3)).explicitGet(),
      "field2" -> CONST_STRING("str").explicitGet(),
      "field3" -> CONST_LONG(5),
      "field4" -> CONST_BOOLEAN(true)
    )
  )

  "roundtrip" - {
    "CONST_LONG" in roundTripTest(CONST_LONG(1))
    "CONST_BYTESTR" in roundTripTest(CONST_BYTESTR(ByteStr.fromBytes(1)).explicitGet())
    "CONST_STRING" in roundTripTest(CONST_STRING("foo").explicitGet())

    "IF" in roundTripTest(IF(TRUE, CONST_LONG(0), CONST_LONG(1)))

    "BLOCKV1" in roundTripTest(
      LET_BLOCK(
        let = LET("foo", TRUE),
        body = FALSE
      )
    )

    "BLOCKV2 with LET" in roundTripTest(
      BLOCK(
        dec = LET("foo", TRUE),
        body = FALSE
      )
    )

    "BLOCKV2 with FUNC" in roundTripTest(
      BLOCK(
        FUNC("foo", List("bar", "buz"), CONST_BOOLEAN(true)),
        CONST_BOOLEAN(false)
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

    "CaseObj if allowed" - {
      "simple" in roundTripTest(caseObj, allowObjects = true)
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

  "spec input" in {
    val byteArr   = Array[Byte](1, 113, -1, 63, 0, -1, 127, 0, -1, 39, -1, 87, -41, 50, -111, -38, 12, 1, 0, -19, 101, -128, -1, 54)
    val (r, time) = measureTime(Serde.deserialize(byteArr).map(_._1))

    r shouldBe an[Either[_, _]]
    time should be <= 1000L
  }

  "any input" in {
    forAll(Gen.containerOf[Array, Byte](Arbitrary.arbByte.arbitrary)) { byteArr =>
      val (r, time) = measureTime(Serde.deserialize(byteArr).map(_._1))

      r shouldBe an[Either[_, _]]
      time should be <= 1000L
    }
  }

  "incorrect base64" in {
    def measureBase64Deser(base64: String): Unit = {
      val (r, time) = measureTime(Script.fromBase64String(base64))

      r should produce("arguments too big")
      time should be <= 1000L
    }

    measureBase64Deser("AgQAAAABYgEAAAAEAAAAAAkAAAAA/wACCQAB9wAAAAEFAAAAAWIJAAH3AAAAAQUAAAABYi+LkdA=")
    measureBase64Deser("AgQAAAABYgEAAAAEAAAAAAkAAAAAAAACCQAB9wD/AAEFAAAAAWIJAAH3AAAAAQUAAAABYtKFiCk=")
    measureBase64Deser("AgQAAAABYgEAAAAEAAAAAAkAAAAAAAACCQAB9wAAAAEFAAAAAWIJAAH3AP8AAQUAAAABYpURGZc=")
  }

  "too big string" in {
    val maxString = "a" * Terms.DataEntryValueMax
    val expr1     = Serde.serialize(CONST_STRING(maxString, reduceLimit = false).explicitGet())
    Serde.deserialize(expr1).map(_._1) shouldBe CONST_STRING(maxString)

    val tooBigString = maxString + "a"
    val expr2        = Serde.serialize(CONST_STRING(tooBigString, reduceLimit = false).explicitGet())
    Serde.deserialize(expr2) should produce("String size=32768 exceeds 32767 bytes")
  }

  "too big bytes" in {
    val maxBytes = ("a" * Terms.DataEntryValueMax).getBytes(StandardCharsets.UTF_8)
    val expr1    = Serde.serialize(CONST_BYTESTR(ByteStr(maxBytes)).explicitGet())
    Serde.deserialize(expr1).map(_._1) shouldBe CONST_BYTESTR(ByteStr(maxBytes))

    val tooBigBytes = maxBytes :+ (1: Byte)
    val expr2       = Serde.serialize(CONST_BYTESTR(ByteStr(tooBigBytes), limit = CONST_BYTESTR.DataTxSize).explicitGet())
    Serde.deserialize(expr2) should produce("ByteStr size=32768 exceeds 32767 bytes")
  }

  "forbid CaseObj" in {
    Try(Serde.serialize(caseObj)).toEither shouldBe Symbol("left")

    val objectBytes = Serde.serialize(caseObj, allowObjects = true)
    Serde.deserialize(objectBytes) shouldBe Symbol("left")
  }

  def measureTime[A](f: => A): (A, Long) = {
    val start  = System.currentTimeMillis()
    val result = f
    (result, System.currentTimeMillis() - start)
  }

  private def roundTripTest(untypedExpr: Expressions.EXPR): Assertion = {
    val typedExpr = ExpressionCompiler(PureContext.build(V1).compilerContext, untypedExpr).map(_._1).explicitGet()
    roundTripTest(typedExpr)
  }

  private def roundTripTest(typedExpr: EXPR, allowObjects: Boolean = false): Assertion = {
    val encoded = Serde.serialize(typedExpr, allowObjects)
    encoded.nonEmpty shouldBe true

    val decoded = Serde.deserialize(encoded, all = true, allowObjects).map(_._1).explicitGet()
    withClue(s"encoded bytes: [${encoded.mkString(", ")}]") {
      decoded shouldEqual typedExpr
    }
  }
}
