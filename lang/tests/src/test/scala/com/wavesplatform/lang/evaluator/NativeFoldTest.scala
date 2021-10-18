package com.wavesplatform.lang.evaluator

import com.wavesplatform.lang.directives.values.{StdLibVersion, V6}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext

class NativeFoldTest extends EvaluatorSpec {
  implicit val startVersion: StdLibVersion = V6

  property("sum") {
    eval(
      s"""
         | func sum(a:Int, b:Int) = a + b
         | fold_20([1, 2, 3, 4, 5], 9, sum)
       """.stripMargin
    ) shouldBe Right(CONST_LONG(1 + 2 + 3 + 4 + 5 + 9))
  }

  property("all is odd") {
    eval(
      s"""
         | func checkOdd(acc: Boolean, a: Int) = acc && (a % 2 == 1)
         | fold_20([1, 3, 5, 7], true, checkOdd)
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("limit") {
    PureContext.folds
      .foreach {
        case (limit, _) =>
          eval(
            s"""
             | func sum(a:Int, b:Int) = a + b
             | fold_$limit([${"1," * (limit - 1)}1], 123, sum)
           """.stripMargin
          ) shouldBe Right(CONST_LONG(limit + 123))

          if (limit < 1000)
            eval(
              s"""
               | func sum(a:Int, b:Int) = a + b
               | fold_$limit([${"1," * limit}1], 123, sum)
             """.stripMargin
            ) shouldBe Left(s"List with size ${limit + 1} was passed to function fold_$limit requiring max size $limit")
      }
  }

  property("fold as fold param") {
    eval(
      s"""
         | func sum(a:Int, b:Int) = a + b
         | let arr = [1, 2, 3, 4, 5]
         | fold_20(arr, fold_20(arr, 9, sum), sum)
       """.stripMargin
    ) shouldBe Right(CONST_LONG(9 + 2 * (1 + 2 + 3 + 4 + 5)))
  }

  property("first argument should be list") {
    eval(
      s"""
         | func sum(a:Int, b:Int) = a + b
         | fold_20(1, 9, sum)
       """.stripMargin
    ) shouldBe Left("Compilation failed: Fold first argument should be List[A], but Int found in 34-52")
  }

  property("suitable function is not found") {
    eval(
      s"""
         | func sum(a:Int, b:String) = a
         | fold_20([1], 0, sum)
       """.stripMargin
    ) shouldBe Left("Compilation failed: Can't find suitable function sum(a: Int, b: Int) for fold in 33-53")
    eval(
      s"""
         | func sum(a:String, b:Int) = a
         | fold_20([1], 0, sum)
       """.stripMargin
    ) shouldBe Left("Compilation failed: Can't find suitable function sum(a: Int, b: Int) for fold in 33-53")
    eval(
      s"""
         | func sum(a:String, b:Int) = a
         | fold_20([], 0, sum)
       """.stripMargin
    ) shouldBe Left("Compilation failed: Can't find suitable function sum(a: Int, b: Any) for fold in 33-52")
  }
}
