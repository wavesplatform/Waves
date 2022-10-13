package com.wavesplatform.lang.compiler

import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.PropSpec
import org.scalatest.Assertion

class LetParseErrorTest extends PropSpec {
  property("missing '=' of let definition") {
    assert(
      """
        | let x = 1
        | let y x
      """.stripMargin,
      """Parse error: expected "=", found "x"""",
      17,
      19,
      "y x"
    )
  }

  property("missing 'let' keyword") {
    assert(
      """
        | let x = 1
        | y = x
      """.stripMargin,
      """Parse error: expected "let" or "strict" keyword, found "y"""",
      12,
      13,
      " y"
    )
  }

  property("'let' keyword without definition") {
    assert(
      """
        | let x = 1
        | let
      """.stripMargin,
      """Parse error: expected variable name""",
      13,
      15,
      "let",
      endExpr = false
    )
  }

  property("missing variable name") {
    assert(
      """
        | let x = 1
        | let = 1
      """.stripMargin,
      """Parse error: expected variable name""",
      13,
      15,
      "let"
    )
  }

  property("missing closing curly brace of let body") {
    assert(
      """
        | let x = {
        |   true
        |
      """.stripMargin,
      """Parse error: expected "}"""",
      26,
      26,
      " ",
      endExpr = false
    )
  }

  property("missing result expression") {
    assert(
      """
        | let x = {
        |   func f() = { 1 }
        | }
      """.stripMargin,
      """Parse error: expected expression""",
      30,
      30,
      "}"
    )
  }

  private def assert(script: String, error: String, start: Int, end: Int, highlighting: String, endExpr: Boolean = true): Assertion = {
    val fullError = s"$error in $start-$end"
    val expr      = if (endExpr) script + "\ntrue" else script
    TestCompiler(V6).compile(script) shouldBe Left(fullError)
    TestCompiler(V6).compileExpressionE(expr) shouldBe Left(fullError)
    script.slice(start, end + 1) shouldBe highlighting
  }
}
