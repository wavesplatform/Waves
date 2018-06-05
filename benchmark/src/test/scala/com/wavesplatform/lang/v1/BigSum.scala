package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.Terms.{BOOLEAN, LONG}
import org.openjdk.jmh.annotations.{Scope, State}

@State(Scope.Benchmark)
class BigSum {
  private val bigSum = (1 to 100).foldLeft[EXPR](CONST_LONG(0)) { (r, i) =>
    FUNCTION_CALL(
      function = FunctionHeader(name = "l+l"),
      args = List(r, CONST_LONG(i))
    )
  }

  val expr: EXPR = FUNCTION_CALL(
    function = FunctionHeader(name = "l=l"),
    args = List(CONST_LONG(1), bigSum)
  )
}
