package com.wavesplatform.lang

import cats.Id
import com.google.common.cache.CacheBuilder
import com.wavesplatform.common.utils.EitherExt2.explicitGet
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, StdLibVersion}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BIGINT, CONST_LONG, EXPR, FUNCTION_CALL}
import com.wavesplatform.lang.v1.evaluator.ContractEvaluator.LogExtraInfo
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import com.wavesplatform.lang.v1.evaluator.FunctionIds.POW_BIGINT
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.Rounding
import com.wavesplatform.lang.v1.traits.Environment

import java.util.concurrent.{Callable, ThreadLocalRandom}

package object v1 {
  def randomBytes(length: Int): Array[Byte] = {
    val bytes = new Array[Byte](length)
    ThreadLocalRandom.current().nextBytes(bytes)
    bytes
  }

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

  private val evCtxCache = CacheBuilder
    .newBuilder()
    .build[StdLibVersion, EvaluationContext[Environment, Id]]()

  def eval(
      expr: EXPR,
      stdLibVersion: StdLibVersion = StdLibVersion.VersionDic.latest
  ): Terms.EVALUATED =
    EvaluatorV2
      .applyCompleted(
        evCtxCache.get(
          stdLibVersion,
          () =>
            lazyContexts((DirectiveSet(StdLibVersion.VersionDic.latest, Account, Expression).explicitGet(), true, true, true))()
              .evaluationContext(Common.emptyBlockchainEnvironment())
        ),
        expr,
        LogExtraInfo(),
        stdLibVersion,
        newMode = true,
        correctFunctionCallScope = true,
        enableExecutionLog = false,
        fixedThrownError = true
      )
      ._3
      .explicitGet()

  def eval(
      ctx: EvaluationContext[Environment, Id],
      expr: EXPR,
      stdLibVersion: StdLibVersion
  ): Terms.EVALUATED =
    EvaluatorV2
      .applyCompleted(
        ctx,
        expr,
        LogExtraInfo(),
        stdLibVersion,
        newMode = true,
        correctFunctionCallScope = true,
        enableExecutionLog = false,
        fixedThrownError = true
      )
      ._3
      .explicitGet()
}
