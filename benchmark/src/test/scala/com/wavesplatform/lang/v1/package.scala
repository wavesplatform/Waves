package com.wavesplatform.lang

import cats.Id
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.miniev.{Ev, State}
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BIGINT, CONST_LONG, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.LogExtraInfo
import com.wavesplatform.lang.v1.evaluator.FunctionIds.POW_BIGINT
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.Rounding
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV2, Log}

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

  def eval(
      ctx: EvaluationContext[Id],
      expr: EXPR,
      stdLibVersion: StdLibVersion = StdLibVersion.VersionDic.all.max
  ): (Log[Id], Int, Either[ExecutionError, Terms.EVALUATED]) =
    EvaluatorV2.applyCompleted(
      ctx,
      expr,
      LogExtraInfo(),
      stdLibVersion,
      newMode = true,
      correctFunctionCallScope = true,
      enableExecutionLog = false,
      fixedThrownError = true
    )

  def miniEv(expr: EXPR, ctx: EvaluationContext[Id], limit: Int = Int.MaxValue): (Log[Id], Int, Either[ExecutionError, Terms.EVALUATED]) =
    Ev.run(expr, ???)
}
