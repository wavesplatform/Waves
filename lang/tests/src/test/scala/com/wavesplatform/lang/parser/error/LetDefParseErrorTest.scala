package com.wavesplatform.lang.parser.error

class LetDefParseErrorTest extends ParseErrorTest {
  property("missing '=' of let definition") {
    assert(
      """
        | let x = 1
        | let y x
      """.stripMargin,
      """Parse error: expected "="""",
      17,
      19,
      "y "
    )
  }

  property("missing 'let' keyword") {
    assert(
      """
        | let x = 1
        | y = x
      """.stripMargin,
      """Parse error: expected "let" or "strict" keyword""",
      13,
      14,
      "y"
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
      16,
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
      16,
      "let"
    )
  }

  property("missing let body") {
    assert(
      """
        | let x = 1
        | let y =
      """.stripMargin,
      """Parse error: expected let body""",
      19,
      27,
      "=\n      ",
      endExpr = false
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
      18,
      27,
      "e\n\n      ",
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
      31,
      "}"
    )
  }
}
