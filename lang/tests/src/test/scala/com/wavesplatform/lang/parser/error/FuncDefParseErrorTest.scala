package com.wavesplatform.lang.parser.error

class FuncDefParseErrorTest extends ParseErrorTest {
  property("missing opening brace of function arguments definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f a: Int) = a
      """.stripMargin,
      """Parse error: expected "("""",
      29,
      31,
      "f "
    )
  }

  property("missing closing brace of function arguments definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a: Int = a
      """.stripMargin,
      """Parse error: expected ")"""",
      36,
      38,
      "t "
    )
  }

  property("missing '=' of function arguments definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a: Int) a
      """.stripMargin,
      """Parse error: expected "="""",
      37,
      39,
      ") "
    )
  }

  property("Java style type definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(Int a) = a
      """.stripMargin,
      """Parse error: expected ":"""",
      33,
      35,
      "t "
    )
  }

  property("missing arguments types") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a, b, c) = a
      """.stripMargin,
      """Parse error: expected ":"""",
      29,
      32,
      "f(a"
    )
  }

  property("showing first of multiple errors") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a Int, b: String, c) a
      """.stripMargin,
      """Parse error: expected ":"""",
      31,
      33,
      "a "
    )
  }

  property("missing 'func' keyword") {
    assert(
      """
        | let x = 1
        | let y = 1
        | f(a: Int) = a
      """.stripMargin,
      """Parse error: expected "func" keyword""",
      24,
      28,
      "f(a:"
    )
  }

  property("'func' keyword without definition") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func
      """.stripMargin,
      """Parse error: expected function name""",
      24,
      28,
      "func",
      endExpr = false
    )
  }

  property("missing function name") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func (a: Int) = a
      """.stripMargin,
      """Parse error: expected function name""",
      24,
      28,
      "func"
    )
  }

  property("missing function body") {
    assert(
      """
        | let x = 1
        | let y = 1
        | func f(a: Int) =
      """.stripMargin,
      """Parse error: expected function body""",
      39,
      47,
      "=\n      ",
      endExpr = false
    )
  }

  property("missing closing curly brace of function body") {
    assert(
      """
        | func f(a: Int) = {
        |   true
        |
      """.stripMargin,
      """Parse error: expected "}"""",
      27,
      36,
      "e\n\n      ",
      endExpr = false
    )
  }

  property("missing result expression") {
    assert(
      """
        | func f() = {
        |   let a = 1
        | }
      """.stripMargin,
      """Parse error: expected expression""",
      26,
      27,
      "1"
    )
  }
}
