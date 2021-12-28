package com.wavesplatform.lang.evaluator.fold

import com.wavesplatform.lang.directives.values.{StdLibVersion, V6}
import com.wavesplatform.lang.evaluator.EvaluatorSpec
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext

class NativeFoldTest extends EvaluatorSpec {
  implicit val startVersion: StdLibVersion = V6

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

  property("list filter") {
    eval(
      s"""
         | func filter(a: List[Int], b: Int) = if b % 2 == 0 then a ++ [b] else a
         | fold_20([${(1 to 13).mkString(",")}], [], filter) == [${(2 to 12 by 2).mkString(",")}]
       """.stripMargin
    ) shouldBe Right(CONST_BOOLEAN(true))
  }

  property("evaluation cost is proportional to list size") {
    def script(limit: Int, size: Int) =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | fold_$limit([${(1 to size).mkString(",")}], 0, sum)
       """.stripMargin

    for {
      (limit, fold) <- PureContext.folds
      listSize      <- List(0, limit / 2, limit)
    } {
      val foldCost       = fold.costByLibVersion(V6)
      val (result, cost) = evalWithCost(script(limit, listSize))
      result shouldBe CONST_LONG((1 to listSize).sum)
      cost shouldBe foldCost + listSize * 2 // 1 for '+' and 1 for list append
    }
  }

  property("evaluation cost depends from branch") {
    val script =
      s"""
         | func g1() = [1].median()
         | func g2() = pow(1, 1, 1, 1, 1, DOWN)
         |
         | func f(acc: Int, elem: Int) =
         |   match elem {
         |     case 1 => g1()
         |     case 2 => g2()
         |     case 3 => 1234
         |     case _ => throw()
         |   }
         |
         | fold_20([1, 2, 3], 0, f)
       """.stripMargin

    val (result, cost) = evalWithCost(script)
    result shouldBe CONST_LONG(1234)
    cost shouldBe
        3 +  // [1, 2, 3]
        1 +  // [1]
        20 + // median
        28 + // pow
        3  + // fold
        6    // match case 3 comparisons: 1 + 2 + 3
  }
}
