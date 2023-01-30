package com.wavesplatform.lang.parser.error

class RoundBraceParseErrorTest extends ParseErrorTest {
  property("missing closing round brace of expression") {
    assert(
      """
        | let a = 1
        | let b = (2 - (a + 1) / 3
        | let c = a + b
      """.stripMargin,
      """Parse error: expected ")"""",
      36,
      39,
      "3\n "
    )
  }

  property("missing closing round brace of function call") {
    assert(
      """
        | func f(a: Int, b: Int) = a + b
        | let c = f(1, 2
        | func g() = c
      """.stripMargin,
      """Parse error: expected ")"""",
      47,
      50,
      "2\n "
    )
  }

  property("missing closing round brace of function call without arguments") {
    assert(
      """
        | func f() = 0
        | let a = f(
        | func g() = a
      """.stripMargin,
      """Parse error: expected ")"""",
      24,
      26,
      "f(",
      endExpr = false
    )
  }

  property("missing closing round brace of OOP style call") {
    assert(
      """
        | func f(a: Int, b: Int) = a + b
        | let c = 1.f(2
        | func g() = c
      """.stripMargin,
      """Parse error: expected ")"""",
      46,
      49,
      "2\n "
    )
  }

  property("missing closing round brace of OOP style call without arguments") {
    assert(
      """
        | let c = "".parseInt(
        | func g() = c
      """.stripMargin,
      """Parse error: expected ")"""",
      21,
      24,
      "(\n ",
      endExpr = false
    )
  }

  property("missing closing round brace of FOLD macro") {
    assert(
      """
        | func sum(a:Int, b:Int) = a + b
        | let arr = [1, 2, 3, 4, 5]
        | let r = FOLD<5>(arr, 9, sum
        | func f() = 1
      """.stripMargin,
      """Parse error: expected ")"""",
      87,
      90,
      "m\n "
    )
  }

  property("missing closing round brace of tuple value definition") {
    assert(
      """
        | let a = (1, 2
        | let b = 1
      """.stripMargin,
      """Parse error: expected ")"""",
      14,
      17,
      "2\n "
    )
  }

  property("missing closing round brace of tuple type definition") {
    assert(
      """
        | func f(a: Int, b: (Int, (String, List[Int]), c: List[Boolean]) = 1
        | let b = 1
      """.stripMargin,
      """Parse error: expected ")"""",
      47,
      48,
      "c"
    )
  }

  property("missing closing round brace of tuple destructure") {
    assert(
      """
        | let (a, b, c = (1, 2, 3)
        | func f() = 1
      """.stripMargin,
      """Parse error: expected ")"""",
      13,
      15,
      "c "
    )
  }

  property("missing closing round brace of annotation argument") {
    assert(
      """
        | @Callable(i
        | func f() = []
      """.stripMargin,
      """Parse error: expected ")"""",
      12,
      15,
      "i\n ",
      onlyDApp = true
    )
  }

  property("missing closing round brace of tuple match pattern") {
    assert(
      """
        | let x =
        |   match (1, (base58'', if (true) then 1 else "")) {
        |     case (_: Int, (_, _: String) => true
        |     case _                       => throw()
        |   }
      """.stripMargin,
      """Parse error: expected ")"""",
      95,
      97,
      ") "
    )
  }

  property("missing closing round brace of object match pattern") {
    assert(
      """
        | let x =
        |   match Address(base58'') {
        |     case Address(bytes = base58'' => true
        |     case _                        => throw()
        |   }
      """.stripMargin,
      """Parse error: expected ")"""",
      72,
      74,
      "' "
    )
  }
}
