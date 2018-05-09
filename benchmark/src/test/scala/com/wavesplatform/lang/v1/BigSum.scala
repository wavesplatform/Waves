package com.wavesplatform.lang.v1

import com.wavesplatform.lang.v1.FunctionHeader.{FunctionHeaderType => FHT}
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_LONG, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.compiler.Terms.{BOOLEAN, LONG, Typed}
import org.openjdk.jmh.annotations.{Scope, State}

@State(Scope.Benchmark)
class BigSum {
  private val bigSum = (1 to 100).foldLeft[EXPR](CONST_LONG(0)) { (r, i) =>
    FUNCTION_CALL(
      function = FunctionHeader(name = "+", List(FHT.LONG, FHT.LONG)),
      args = List(r, CONST_LONG(i)),
      LONG
    )
  }

  val expr: EXPR = FUNCTION_CALL(
    function = FunctionHeader(name = "==", List(FHT.LONG, FHT.LONG)),
    args = List(CONST_LONG(1), bigSum),
    BOOLEAN
  )
}
