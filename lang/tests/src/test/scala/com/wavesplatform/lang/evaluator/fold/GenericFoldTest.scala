package com.wavesplatform.lang.evaluator.fold

import com.wavesplatform.lang.directives.values.{V3, V6}
import com.wavesplatform.lang.evaluator.EvaluatorSpec
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG}
import com.wavesplatform.test._
import org.scalatest.Assertion

class GenericFoldTest extends EvaluatorSpec {
  private def eval(script: String => String): Either[String, Terms.EVALUATED] = {
    val r1 = eval(script("FOLD<20>"))(V3)
    val r2 = eval(script("fold_20"))(V6)
    r1 shouldBe r2
    r2
  }

  private def assertError(script: String => String, error: String => String): Assertion = {
    eval(script("FOLD<20>"))(V3) should produce (error("FOLD<20>"))
    eval(script("fold_20"))(V6) should produce (error("fold_20"))
  }

  property("sum") {
    eval(fold =>
      s"""
         | func sum(a:Int, b:Int) = a + b
         | $fold([1, 2, 3, 4, 5], 9, sum)
       """.stripMargin
    ) shouldBe Right(CONST_LONG(1 + 2 + 3 + 4 + 5 + 9))
  }

  property("all is odd") {
    eval(fold =>
      s"""
         | func checkOdd(acc: Boolean, a: Int) = acc && (a % 2 == 1)
         | $fold([1, 3, 5, 7], true, checkOdd)
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("fold as fold param") {
    eval(fold =>
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [1, 2, 3, 4, 5]
         | $fold(arr, $fold(arr, 9, sum), sum)
       """.stripMargin
    ) shouldBe Right(CONST_LONG(9 + 2 * (1 + 2 + 3 + 4 + 5)))
  }

  property("first argument should be list") {
    assertError(fold =>
      s"""
         | func sum(a:Int, b:Int) = a + b
         | $fold(1, 9, sum)
       """.stripMargin,
      fold => s"Compilation failed: First $fold argument should be List[A], but Int found"
    )
  }

  property("suitable function is not found") {
    assertError(fold =>
      s"""
         | func sum(a:Int, b:String) = a
         | $fold([1], 0, sum)
       """.stripMargin,
      fold => s"Compilation failed: Can't find suitable function sum(a: Int, b: Int) for $fold"
    )

    assertError(fold =>
      s"""
         | func sum(a:String, b:Int) = a
         | $fold([1], 0, sum)
       """.stripMargin,
      fold => s"Compilation failed: Can't find suitable function sum(a: Int, b: Int) for $fold"
    )

    assertError(fold =>
      s"""
         | func sum(a:String, b:Int) = a
         | $fold([], 0, sum)
       """.stripMargin,
      fold => s"Compilation failed: Can't find suitable function sum(a: Int, b: Any) for $fold"
    )
  }
}
