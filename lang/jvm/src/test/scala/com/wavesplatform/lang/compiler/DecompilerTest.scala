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
    Decompiler(expr) shouldBe """if (true) then 1 else "XXX""""
  }

  property("simple let") {
    val expr = Terms.BLOCKV1(LET("a", CONST_LONG(1)), TRUE)
    Decompiler(expr) shouldBe "let a = 1; true"
  }

  property("native function call without args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.Native(1),
      args = List(TRUE)
    )
    Decompiler(expr) shouldBe "Native_1(true)"
  }

  property("user function call without args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List(TRUE)
    )
    Decompiler(expr) shouldBe "foo(true)"
  }

  property("user function call with empty args") {
    val expr = Terms.FUNCTION_CALL(
      function = FunctionHeader.User("foo"),
      args = List.empty
    )
    Decompiler(expr) shouldBe "foo()"
  }

  property("user function declaration") {
    val expr = Terms.FUNC("foo", List("bar", "buz"), CONST_BOOLEAN(true))
    Decompiler(expr) shouldBe "func foo (bar,buz) = TRUE"
  }


  property("v2 with LET") {
    val expr = Terms.BLOCKV2(
      LET("vari", REF("p")),
      TRUE
    )
    Decompiler(expr) shouldBe "let vari = p; true"
  }

  property("match") {
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
    Decompiler(expr) shouldBe """let v = p; if (if (Native_1(v,"a")) then true else Native_1(v,"b")) then let p = v; true else false"""
  }

}