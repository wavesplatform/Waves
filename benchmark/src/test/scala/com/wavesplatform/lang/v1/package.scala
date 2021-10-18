package com.wavesplatform.lang

import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BIGINT, CONST_LONG, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.FunctionIds.POW_BIGINT
import com.wavesplatform.lang.v1.evaluator.ctx.impl.Rounding

package object v1 {
  def pow(base: BigInt, basePrecision: Int, exponent: BigInt, exponentPrecision: Int, resultPrecision: Int): EXPR =
    FUNCTION_CALL(
      Native(POW_BIGINT),
      List(
        CONST_BIGINT(base),
        CONST_LONG(basePrecision),
        CONST_BIGINT(exponent),
        CONST_LONG(exponentPrecision),
        CONST_LONG(resultPrecision),
        Rounding.Down.value
      )
    )
}
