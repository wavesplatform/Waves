package com.wavesplatform.lang.evaluator.fold

import com.wavesplatform.lang.directives.values.{StdLibVersion, V6}
import com.wavesplatform.lang.evaluator.EvaluatorSpec
import com.wavesplatform.lang.v1.compiler.Terms.CONST_LONG
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
}
