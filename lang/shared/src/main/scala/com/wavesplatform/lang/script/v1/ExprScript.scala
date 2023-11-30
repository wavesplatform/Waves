package com.wavesplatform.lang.script.v1

import cats.instances.either._
import cats.syntax.either._
import cats.syntax.flatMap._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.BaseGlobal
import com.wavesplatform.lang.v1.ContractLimits._
import com.wavesplatform.lang.v1.compiler.ContractCompiler
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import monix.eval.Coeval

object ExprScript {

  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA

  val checksumLength = 4

  def validateBytes(bs: Array[Byte], isFreeCall: Boolean): Either[String, Unit] = {
    val limit = if (isFreeCall) MaxContractSizeInBytes else MaxExprSizeInBytes
    Either.cond(
      bs.length <= limit,
      (),
      s"Script is too large: ${bs.length} bytes > $limit bytes"
    )
  }
  def apply(x: EXPR): Either[String, Script] = apply(V1, x)

  def apply(version: StdLibVersion, x: EXPR, isFreeCall: Boolean = false, checkSize: Boolean = true): Either[String, ExprScript] =
    ExprScriptImpl(version, isFreeCall, x)
      .asRight[String]
      .flatTap(s => if (checkSize) validateBytes(s.bytes().arr, isFreeCall) else Right(()))

  def estimateExact(
      expr: EXPR,
      version: StdLibVersion,
      isFreeCall: Boolean,
      estimator: ScriptEstimator,
      withCombinedContext: Boolean = false
  ): Either[String, Long] = {
    val modifiedExpr = if (version < V6) {
      expr
    } else {
      BLOCK(LET(ContractCompiler.FreeCallInvocationArg, TRUE), expr)
    }
    val resultVarNames = if (withCombinedContext) combinedVarNames(version, Expression) else varNames(version, Expression)
    estimator(
      resultVarNames,
      functionCosts(version, Expression, if (isFreeCall) Call else Account, withCombinedContext = withCombinedContext),
      modifiedExpr
    )
  }

  def estimate(
      expr: EXPR,
      version: StdLibVersion,
      isFreeCall: Boolean,
      estimator: ScriptEstimator,
      useContractVerifierLimit: Boolean,
      withCombinedContext: Boolean = false
  ): Either[String, Long] =
    for {
      complexity <- estimateExact(expr, version, isFreeCall, estimator, withCombinedContext)
      _          <- checkComplexity(version, complexity, useContractVerifierLimit, isFreeCall)
    } yield complexity

  def checkComplexity(version: StdLibVersion, complexity: Long, useContractVerifierLimit: Boolean, isFreeCall: Boolean): Either[String, Unit] = {
    val limit =
      if (isFreeCall)
        MaxCallableComplexityByVersion(version)
      else if (useContractVerifierLimit)
        MaxAccountVerifierComplexityByVersion(version)
      else
        MaxComplexityByVersion(version)

    Either.cond(
      complexity <= limit,
      (),
      s"Script is too complex: $complexity > $limit"
    )
  }

  final case class ExprScriptImpl(stdLibVersion: StdLibVersion, isFreeCall: Boolean, expr: EXPR) extends ExprScript {
    override type Expr = EXPR
    override val bytes: Coeval[ByteStr]           = Coeval.evalOnce(ByteStr(Global.serializeExpression(expr, stdLibVersion)))
    override val containsBlockV2: Coeval[Boolean] = Coeval.evalOnce(com.wavesplatform.lang.v1.compiler.containsBlockV2(expr))
    override val containsArray: Boolean           = com.wavesplatform.lang.v1.compiler.containsArray(expr)
  }
}

trait ExprScript extends Script {
  override type Expr = EXPR
  val stdLibVersion: StdLibVersion
  val isFreeCall: Boolean
  val expr: EXPR
}
