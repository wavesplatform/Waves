package com.wavesplatform.lang.evaluator.fold

import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.evaluator.EvaluatorSpec
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG

class FoldTest extends EvaluatorSpec {
  implicit val startVersion: StdLibVersion = V3

  property("limit") {
    val script =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [1, 2, 3, 4, 5]
         | let total = FOLD<4>(arr, 9, sum)
         | total
         |
      """.stripMargin

    eval(script) shouldBe Left("List size exceeds 4")
  }

  property("limit for limit") {
    val script =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [1, 2, 3, 4, 5]
         | let total = FOLD<1001>(arr, 9, sum)
         | total
         |
      """.stripMargin

    val index = script.indexOf("1001")
    eval(script) shouldBe Left(s"Compilation failed: [List size limit in FOLD is too big, 1001 must be less or equal 1000 in $index-${index + 4}]")
  }

  property("maximum limit") {
    val script =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [${"1," * 999}1]
         | let total = FOLD<1000>(arr, 2, sum)
         | total
         |
      """.stripMargin

    eval(script) shouldBe Right(CONST_LONG(1002L))
  }
}
