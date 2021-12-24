package com.wavesplatform.lang.v1.estimator

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.StdLibVersion.V3
import com.wavesplatform.lang.directives.values.{StdLibVersion, V6}
import com.wavesplatform.lang.utils.functionCosts
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, CONST_LONG, CONST_STRING, FUNCTION_CALL, REF}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.test.produce

class NativeFoldEstimatorTest extends ScriptEstimatorTestBase(ScriptEstimatorV3(fixOverflow = true, overhead = true)) {
  private def estimateNoOverhead(script: String): Either[ExecutionError, Long] =
    ScriptEstimatorV3(fixOverflow = true, overhead = false)(lets, functionCosts(V6), compile(script)(V6))

  property("fold costs") {
    val foldCostByLimit = Map(20 -> 3, 50 -> 7, 100 -> 9, 200 -> 20, 500 -> 56, 1000 -> 115)
    def script(limit: Int) =
      s"""
         | func sum(a:Int, b:Int) = a + b
         | fold_$limit([], 0, sum)
       """.stripMargin

    PureContext.folds
      .foreach {
        case (limit, _) =>
        /*
          cost(sum)   = 3
          cost([])    = 1
          cost("sum") = 1
          cost(0)     = 1
          total = limit * cost(sum) + cost([]) + cost("sum") + cost(0) + cost(fold) = 3 * (limit + 1) + cost(fold)
        */
        estimate(script(limit)) shouldBe Right(3 * (limit + 1) + foldCostByLimit(limit))

        /*
          cost(sum)   = 1
          cost([])    = 0
          cost("sum") = 0
          cost(0)     = 0
          total = limit * cost(sum) + cost(fold) = limit + cost(fold)
        */
        estimateNoOverhead(script(limit)) shouldBe Right(limit + foldCostByLimit(limit))
      }
  }

  property("unexisting function") {
    PureContext.folds
      .foreach {
        case (limit, f) =>
          estimate(
            functionCosts(V6),
            FUNCTION_CALL(f.header, List(REF("nil"), CONST_LONG(0), CONST_STRING("sum").explicitGet()))
          ) shouldBe Left(s"Unexpected call of high-order function fold_$limit: 'sum' is not found in the scope")
      }
  }

  property("missing argument") {
    PureContext.folds
      .foreach {
        case (limit, f) =>
          estimate(
            functionCosts(V6),
            FUNCTION_CALL(f.header, List(REF("nil"), CONST_LONG(0)))
          ) shouldBe Left(s"Unexpected call of high-order function fold_$limit: only 2 args passed while 3 expected")
      }
  }

  property("arbitrary argument instead of function reference") {
    PureContext.folds
      .foreach {
        case (limit, f) =>
          estimate(
            functionCosts(V6),
            FUNCTION_CALL(f.header, List(REF("nil"), CONST_LONG(0), CONST_BOOLEAN(true)))
          ) shouldBe Left(s"Unexpected call of high-order function fold_$limit: expression 'true' is passed as function reference")
      }
  }

  property("fold is unavailable for previous versions") {
    for {
      version   <- DirectiveDictionary[StdLibVersion].all.filter(v => v >= V3 && v < V6)
      (_, fold) <- PureContext.folds
    } {
      estimate(
        functionCosts(version),
        FUNCTION_CALL(fold.header, List(REF("nil"), CONST_LONG(0), CONST_STRING("f").explicitGet()))
      ) should produce(s"function '${fold.header}' not found")
    }
  }
}
