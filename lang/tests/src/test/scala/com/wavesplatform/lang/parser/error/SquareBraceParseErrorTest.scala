package com.wavesplatform.lang.parser.error

class SquareBraceParseErrorTest extends ParseErrorTest {
  property("missing closing square brace of list definition") {
    assert(
      """
        | let x = [1, 2, 3
        | func f() = true
      """.stripMargin,
      """Parse error: expected "]"""",
      19,
      23,
      " func"
    )
  }

  property("missing closing square brace of deep list definition") {
    assert(
      """
        | let x = [[1], 2, [, [[[]]]
        | func f() = true
      """.stripMargin,
      """Parse error: expected "]"""",
      16,
      20,
      "2, [,"
    )
  }

  property("missing closing square brace of accessing by index") {
    assert(
      """
        | let list = [1, 2, 3]
        | let a = list[1
        | func f() = true
      """.stripMargin,
      """Parse error: expected "]"""",
      39,
      43,
      " func"
    )
  }

  property("missing closing square brace of accessing by expression index") {
    assert(
      """
        | let a = [[1]][[1, 2, 3][1]
        | func f() = true
      """.stripMargin,
      """Parse error: expected "]"""",
      29,
      33,
      " func"
    )
  }

  property("missing closing square brace of generic type") {
    assert(
      """func f(a: List[List[String]|Int, b: ByteVector) = true""",
      """Parse error: expected "]"""",
      5,
      31,
      "f(a: List[List[String]|Int,"
    )
  }

  property("missing closing square brace of generic call") {
    assert(
      """
        | let a = (if true then 1 else unit).exactAs[Int
        | func f() = 1
      """.stripMargin,
      """Parse error: expected "]"""",
      49,
      53,
      " func"
    )
  }
}
