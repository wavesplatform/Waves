package com.wavesplatform.lang.parser.error

class CurlyBraceParseErrorTest extends ParseErrorTest {
  property("missing closing curly brace of match block") {
    assert(
      """
        | let a = if (true) then 1 else ""
        | let b = match a {
        |   case _: Int    => throw()
        |   case _: String => throw()
        |
        | func f() = 1
      """.stripMargin,
      """Parse error: expected "}"""",
      113,
      117,
      " func"
    )
  }

  property("missing closing curly brace of case block") {
    assert(
      """
        | let a = if (true) then 1 else ""
        | let b = match a {
        |   case _: Int => {
        |     let x = 1
        |     let y = 1
        |     throw()
        |   case _: String => throw()
        | }
      """.stripMargin,
      """Parse error: expected "}"""",
      119,
      123,
      " case"
    )
  }

  property("missing closing curly brace of condition block") {
    assert(
      """
        | let a =
        |   if {
        |     let x = 1
        |     let y = 1
        |     y == x
        |   then 1
        |   else ""
        | func f() = true
      """.stripMargin,
      """Parse error: expected "}"""",
      62,
      66,
      " then"
    )
  }

  property("missing closing curly brace of then block") {
    assert(
      """
        | let a =
        |   if {
        |     let x = 1
        |     let y = 1
        |     y == x
        |   }
        |   then {
        |     func g() = throw()
        |     g()
        |   else ""
        | func f() = true
      """.stripMargin,
      """Parse error: expected "}"""",
      110,
      114,
      " else"
    )
  }

  property("missing closing curly brace of else block") {
    assert(
      """
        | let a =
        |   if {
        |     let x = 1
        |     let y = 1
        |     y == x
        |   }
        |   then {
        |     func g() = throw()
        |     g()
        |   } else {
        |     func h() = throw()
        |     {if {h()} then {h()} else {h()}}
        |
        | func f() = true
      """.stripMargin,
      """Parse error: expected "}"""",
      183,
      187,
      " func"
    )
  }
}
