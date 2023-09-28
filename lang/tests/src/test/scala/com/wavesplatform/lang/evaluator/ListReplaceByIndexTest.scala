package com.wavesplatform.lang.evaluator

import com.wavesplatform.lang.directives.values.{StdLibVersion, V8}
import com.wavesplatform.lang.v1.compiler.Terms.CONST_BOOLEAN
import com.wavesplatform.test.produce

class ListReplaceByIndexTest extends EvaluatorSpec {
  private implicit val version: StdLibVersion = V8

  property("successful results") {
    val bigList = (1 to 1000).map(_ => """"a"""").mkString("[", ",", "]")
    eval(
      s"""
         | [0].replaceByIndex(0, 9)               == [9]             &&
         | [0, 1, 2, 3, 4].replaceByIndex(0, 9)   == [9, 1, 2, 3, 4] &&
         | [0, 1, 2, 3, 4].replaceByIndex(2, 9)   == [0, 1, 9, 3, 4] &&
         | [0, 1, 2, 3, 4].replaceByIndex(4, 9)   == [0, 1, 2, 3, 9] &&
         | $bigList.replaceByIndex(0, "b")[0]     == "b"             && # can't compare the whole list due to
         | $bigList.replaceByIndex(500, "b")[500] == "b"             && # "Comparable value too heavy" error
         | $bigList.replaceByIndex(999, "b")[999] == "b"
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("errors") {
    eval("[].replaceByIndex(0, 1)") should produce("Can't replace an element in empty list")
    eval("[1, 2, 3].replaceByIndex(-1, 1)") should produce("Index of the replacing element should be positive, but -1 was passed")
    eval("[1, 2, 3].replaceByIndex(3, 1)") should produce("Index of the replacing element should be lower than list size = 3, but 3 was passed")
    eval("[1, 2, 3].replaceByIndex(2, true)") should produce("Compilation failed: [Can't match inferred types of T over Int, Boolean in 0-33]")
  }
}
