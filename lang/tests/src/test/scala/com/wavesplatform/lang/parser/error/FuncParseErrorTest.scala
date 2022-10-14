package com.wavesplatform.lang.parser.error

class FuncParseErrorTest extends ParseErrorTest {
  property("missing opening brace of function arguments definition") {
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

  property("missing closing brace of function arguments definition") {
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
      """Parse error: expected "func" keyword, found "f(a:"""",
      23,
      27,
      " f(a:"
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
      27,
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
      27,
      "func"
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
      35,
      35,
      " ",
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
      26,
      "1"
    )
  }
}
