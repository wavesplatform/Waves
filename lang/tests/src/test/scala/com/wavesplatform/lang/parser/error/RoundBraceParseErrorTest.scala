package com.wavesplatform.lang.parser.error

class RoundBraceParseErrorTest extends ParseErrorTest {
  property("missing closing round brace of expression") {
    assert(
      """
        | let a = 1
        | let b = (2 - (a + 1) / 3
        | let c = a + b
      """.stripMargin,
      """Parse error: expected ")", found "let"""",
      38,
      41,
      " let"
    )
  }

  property("missing closing round brace of function call") {
    assert(
      """
        | func f(a: Int, b: Int) = a + b
        | let c = f(1, 2
        | func g() = c
      """.stripMargin,
      """Parse error: expected ")", found "func"""",
      49,
      53,
      " func"
    )
  }

  property("missing closing round brace of function call without arguments") {
    assert(
      """
        | func f() = 0
        | let a = f(
        | func g() = a
      """.stripMargin,
      """Parse error: expected ")", found "func"""",
      27,
      31,
      " func",
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
      """Parse error: expected ")", found "func"""",
      48,
      52,
      " func"
    )
  }

  property("missing closing round brace of OOP style call without arguments") {
    assert(
      """
        | let c = "".parseInt(
        | func g() = c
      """.stripMargin,
      """Parse error: expected ")", found "func"""",
      23,
      27,
      " func",
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
      """Parse error: expected ")", found "func"""",
      89,
      93,
      " func"
    )
  }

  property("missing closing round brace of tuple value definition") {
    assert(
      """
        | let a = (1, 2
        | let b = 1
      """.stripMargin,
      """Parse error: expected ")", found "let"""",
      16,
      19,
      " let"
    )
  }

  property("missing closing round brace of tuple type definition") {
    assert(
      """
        | func f(a: Int, b: (Int, (String, List[Int]), c: List[Boolean]) = 1
        | let b = 1
      """.stripMargin,
      """Parse error: expected ")", found ":"""",
      35,
      48,
      "List[Int]), c:"
    )
  }

  property("missing closing round brace of tuple destructure") {
    assert(
      """
        | let (a, b, c = (1, 2, 3)
        | func f() = 1
      """.stripMargin,
      """Parse error: expected ")", found "="""",
      13,
      15,
      "c ="
    )
  }

  property("missing closing round brace of annotation argument") {
    assert(
      """
        | @Callable(i
        | func f() = []
      """.stripMargin,
      """Parse error: expected ")", found "func"""",
      14,
      18,
      " func",
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
      """Parse error: expected ")", found "=>"""",
      89,
      98,
      "String) =>"
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
      """Parse error: expected ")", found "=>"""",
      65,
      75,
      "base58'' =>"
    )
  }
}
