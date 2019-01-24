package com.wavesplatform.lang.compiler

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.{Decompiler, Terms}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks

class DecompilerTest extends PropSpec with PropertyChecks with Matchers {

  property("simple if") {
    val expr = IF(TRUE, CONST_LONG(1), CONST_STRING("XXX"))
    Decompiler(expr, 0) shouldBe
      """if (true) then
        |    1
        |else
        |    "XXX"
        |""".stripMargin
  }

  property("if with complicated else branch") {
    val expr = IF(TRUE, CONST_LONG(1),
      IF(TRUE, CONST_LONG(1),
        CONST_STRING("XXX")))
    Decompiler(expr, 0) shouldBe
      """if (true) then
        |    1
        |else
        |    if (true) then
        |        1
        |    else
        |        "XXX"
        |
        |""".stripMargin
  }

  property("if with complicated then branch") {
    val expr = IF(TRUE,
      IF(TRUE, CONST_LONG(1),
        CONST_STRING("XXX")),
      CONST_LONG(1))
    Decompiler(expr, 0) shouldBe
      """if (true) then
        |    if (true) then
        |        1
        |    else
        |        "XXX"
        |
        |else
        |    1
        |""".stripMargin
  }

  property("simple let") {
    val expr = Terms.BLOCKV1(LET("a", CONST_LONG(1)), TRUE)
    Decompiler(expr, 0) shouldBe "{ let a = 1; true }"
  }

  property("native function call without args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(1),
      args = List(TRUE)
    )
    Decompiler(expr, 0) shouldBe "Native_1(true)"
  }

  property("user function call with one args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List(TRUE)
    )
    Decompiler(expr, 0) shouldBe "foo(true)"
  }

  property("user function call with empty args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List.empty
    )
    Decompiler(expr, 0) shouldBe "foo()"
  }

  property("user function declaration") {
    val expr = Terms.FUNC("foo", List("bar", "buz"), CONST_BOOLEAN(true))
    Decompiler(expr, 0) shouldBe "{ func foo (bar,buz) = TRUE }"
  }

  property("v2 with LET in BLOCK") {
    val expr = Terms.BLOCKV2(
      LET("vari", REF("p")),
      TRUE
    )
    Decompiler(expr, 0) shouldBe
    """{
      |    let vari = p;
      |    true
      |}""".stripMargin
  }

  property("let and function call in block") {
    val expr = Terms.BLOCKV2(
      Terms.LET("v", REF("p")),
      Terms.FUNCTION_CALL(
        PureContext._isInstanceOf.header,
        List(REF("v"), Terms.CONST_STRING("a"))
      ))
    Decompiler(expr, 0) shouldBe
      """{
        |    let v = p;
        |    Native_1(v,"a")
        |}""".stripMargin
  }

  property("complicated let in let and function call in block") {
    val expr = Terms.BLOCKV2(
      Terms.LET("v", Terms.BLOCKV2(
        Terms.LET("v", REF("p")),
        Terms.FUNCTION_CALL(
          PureContext._isInstanceOf.header,
          List(REF("v"), Terms.CONST_STRING("a"))
        ))),
      Terms.FUNCTION_CALL(
        PureContext._isInstanceOf.header,
        List(REF("v"), Terms.CONST_STRING("a"))
      ))
    Decompiler(expr, 0) shouldBe
      """{
        |    let v = {
        |        let v = p;
        |        Native_1(v,"a")
        |    };
        |    Native_1(v,"a")
        |}""".stripMargin
  }


  property("old match") {
    val expr = Terms.BLOCKV2(
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
        BLOCKV2(LET("p", REF("v")), TRUE),
        FALSE
      )
    )
    Decompiler(expr, 0) shouldBe """let v = p; if (if (Native_1(v,"a")) then true else Native_1(v,"b")) then let p = v; true else false"""
  }

  property("new match") {
    val expr = Terms.BLOCKV2(
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
          Terms.BLOCKV2(Terms.LET("p", Terms.REF("v")), TRUE),
          FALSE
        )
      )
    Decompiler(expr, 0) shouldBe
    """{a
      |    let v = p;
      |    if (if (Native_1(v,"a")) then
      |            true
      |        else
      |            Native_1(v,"b")) then
      |            {
      |                let p = v;
      |                true
      |            }
      |        else
      |            false
      |}""".stripMargin


    //"""{ let v = p; if (if (Native_1(v,"a")) then true else Native_1(v,"b")) then { let p = v; true } else false }"""
  }

}