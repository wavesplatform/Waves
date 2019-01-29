package com.wavesplatform.lang.compiler

import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import com.wavesplatform.lang.v1.compiler.{Decompiler, ExpressionCompilerV1, Terms}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class DecompilerTest extends PropSpec with PropertyChecks with Matchers {

  val opcodes = scala.collection.immutable.Map[Int,String](1-> "+", 100->"sigVerify")

  property("simple if") {
    val expr = IF(TRUE, CONST_LONG(1), CONST_STRING("XXX"))
    Decompiler(expr, opcodes) shouldBe
      """{ if (
        |    true
        |    )
        |then
        |    1
        |else
        |    "XXX"
        |}""".stripMargin
  }

  property("if with complicated else branch") {
    val expr = IF(TRUE, CONST_LONG(1),
      IF(TRUE, CONST_LONG(1),
        CONST_STRING("XXX")))
    Decompiler(expr, opcodes) shouldBe
      """{ if (
        |    true
        |    )
        |then
        |    1
        |else
        |    { if (
        |        true
        |        )
        |    then
        |        1
        |    else
        |        "XXX"
        |    }
        |}""".stripMargin
  }

  property("if with complicated then branch") {
    val expr = IF(TRUE,
      IF(TRUE, CONST_LONG(1),
        CONST_STRING("XXX")),
      CONST_LONG(1))
    Decompiler(expr, opcodes) shouldBe
      """{ if (
        |    true
        |    )
        |then
        |    { if (
        |        true
        |        )
        |    then
        |        1
        |    else
        |        "XXX"
        |    }
        |else
        |    1
        |}""".stripMargin
  }

  property("simple let") {
    val expr = Terms.LET_BLOCK(LET("a", CONST_LONG(1)), TRUE)
    Decompiler(expr, opcodes) shouldBe "{ let a = 1; true }"
  }

  property("native function call without args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(100),
      args = List(TRUE)
    )
    Decompiler(expr, opcodes) shouldBe "sigVerify(true)"
  }


  property("undefined native function call without args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(101),
      args = List(TRUE)
    )
    Decompiler(expr, opcodes) shouldBe "<Native_101>(true)"
  }


  property("user function call with one args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List(TRUE)
    )
    Decompiler(expr, opcodes) shouldBe "foo(true)"
  }

  property("user function call with empty args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List.empty
    )
    Decompiler(expr, opcodes) shouldBe "foo()"
  }

  property("user function declaration") {
    val expr = Terms.FUNC("foo", List("bar", "buz"), CONST_BOOLEAN(true))
    Decompiler(expr, opcodes) shouldBe "{ func foo (bar,buz) = TRUE }"
  }

  property("v2 with LET in BLOCK") {
    val expr = Terms.BLOCK(
      LET("vari", REF("p")),
      TRUE
    )
    Decompiler(expr, opcodes) shouldBe
    """{
      |    let vari =
      |        p;
      |    true
      |}""".stripMargin
  }

  property("let and function call in block") {
    val expr = Terms.BLOCK(
      Terms.LET("v", REF("p")),
      Terms.FUNCTION_CALL(
        PureContext._isInstanceOf.header,
        List(REF("v"), Terms.CONST_STRING("a"))
      ))
    Decompiler(expr, opcodes) shouldBe
      """{
        |    let v =
        |        p;
        |    +(v,"a")
        |}""".stripMargin
  }

  property("complicated let in let and function call in block") {
    val expr = Terms.BLOCK(
      Terms.LET("v", Terms.BLOCK(
        Terms.LET("v", REF("p")),
        Terms.FUNCTION_CALL(
          PureContext._isInstanceOf.header,
          List(REF("v"), Terms.CONST_STRING("a"))
        ))),
      Terms.FUNCTION_CALL(
        PureContext._isInstanceOf.header,
        List(REF("v"), Terms.CONST_STRING("a"))
      ))
    Decompiler(expr, opcodes) shouldBe
      """{
        |    let v =
        |        {
        |            let v =
        |                p;
        |            +(v,"a")
        |        };
        |    +(v,"a")
        |}""".stripMargin
  }

  property("old match") {
    val expr = Terms.BLOCK(
      LET("v", REF("p")),
      IF(
        IF(
          FUNCTION_CALL(
            PureContext._isInstanceOf.header,
            List(REF("v"), CONST_STRING("a"))
          ),
          TRUE,
          FUNCTION_CALL(
            PureContext._isInstanceOf.header,
            List(REF("v"), CONST_STRING("b"))
          )
        ),
        BLOCK(LET("p", REF("v")), TRUE),
        FALSE
      )
    )
    Decompiler(expr, opcodes) shouldBe
      """{
        |    let v =
        |        p;
        |    { if (
        |        { if (
        |            +(v,"a")
        |            )
        |        then
        |            true
        |        else
        |            +(v,"b")
        |        }
        |        )
        |    then
        |        {
        |            let p =
        |                v;
        |            true
        |        }
        |    else
        |        false
        |    }
        |}""".stripMargin
  }

  property("new match") {
    val expr = Terms.BLOCK(
      Terms.LET("v", REF("p")),
        Terms.IF(
          Terms.IF(
            Terms.FUNCTION_CALL(
              PureContext._isInstanceOf.header,
              List(REF("v"), Terms.CONST_STRING("a"))
            ),
            TRUE,
            Terms.FUNCTION_CALL(
              PureContext._isInstanceOf.header,
              List(REF("v"), Terms.CONST_STRING("b"))
            )
         ),
          Terms.BLOCK(
            Terms.LET("z", Terms.REF("x")
            ), TRUE),
          FALSE
        )
      )
    Decompiler(expr, opcodes) shouldBe
    """{
      |    let v =
      |        p;
      |    { if (
      |        { if (
      |            +(v,"a")
      |            )
      |        then
      |            true
      |        else
      |            +(v,"b")
      |        }
      |        )
      |    then
      |        {
      |            let z =
      |                x;
      |            true
      |        }
      |    else
      |        false
      |    }
      |}""".stripMargin
  }

}