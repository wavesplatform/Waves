package com.wavesplatform.lang.compiler
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.PropSpec
import org.scalatest.Assertion

class ParseErrorTest extends PropSpec {
  property("missing first brace of function arguments definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f a: Int) = a
      """.stripMargin,
      """Parse error: expected "(", found "a:"""",
      29,
      32,
      "f a:"
    )
  }

  property("missing second brace of function arguments definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a: Int = a
      """.stripMargin,
      """Parse error: expected ")", found "="""",
      34,
      38,
      "Int ="
    )
  }

  property("missing '=' of function arguments definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a: Int) a
      """.stripMargin,
      """Parse error: expected "=", found "a"""",
      34,
      39,
      "Int) a"
    )
  }

  property("missing ':' of function arguments definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a Int) = a
      """.stripMargin,
      """Parse error: expected ":", found "Int)"""",
      29,
      36,
      "f(a Int)"
    )
  }

  property("Java style type definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(Int a) = a
      """.stripMargin,
      """Parse error: expected ":", found "a)"""",
      29,
      36,
      "f(Int a)"
    )
  }

  property("missing arguments types") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a, b, c) = a
      """.stripMargin,
      """Parse error: expected ":", found ","""",
      24,
      32,
      "func f(a,"
    )
  }

  property("showing first of multiple errors") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a Int, b: String, c) a
      """.stripMargin,
      """Parse error: expected ":", found "Int,"""",
      29,
      36,
      "f(a Int,"
    )
  }

  property("missing 'func' keyword") {
    assert(
      """
        | let x = 1
        | let y = 1
        | f(a: Int) = a
      """.stripMargin,
      """Parse error: expected 'func' keyword, found "f(a:"""",
      23,
      27,
      " f(a:"
    )
  }

  private def assert(script: String, error: String, start: Int, end: Int, highlighting: String): Assertion = {
    val fullError = s"$error in $start-$end"
    TestCompiler(V6).compile(script) shouldBe Left(fullError)
    TestCompiler(V6).compileExpressionE(script + "\ntrue") shouldBe Left(fullError)
    script.slice(start, end + 1) shouldBe highlighting
  }
}
