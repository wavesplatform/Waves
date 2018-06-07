package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, EXPR, FUNCTION_CALL}
import org.openjdk.jmh.annotations.{Scope, State}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._

@State(Scope.Benchmark)
class BigSum {
  private val bigSum = (1 to 100).foldLeft[EXPR](CONST_LONG(0)) { (r, i) =>
    FUNCTION_CALL(
      function = FunctionHeader.Predef(SUM_LONG),
      args = List(r, CONST_LONG(i))
    )
  }

  val expr: EXPR = FUNCTION_CALL(
    function = FunctionHeader.Predef(EQ),
    args = List(CONST_LONG(1), bigSum)
  )
}
