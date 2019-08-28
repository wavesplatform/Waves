package com.wavesplatform.lang.script.v1

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.ContractLimits._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.BaseGlobal
import monix.eval.Coeval
import cats.implicits._
import com.google.common.annotations.VisibleForTesting
import com.wavesplatform.lang.v1.estimator.ScriptEstimator

object ExprScript {

  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA

  val checksumLength = 4

  def validateBytes(bs: Array[Byte]): Either[String, Unit] =
    Either.cond(
      bs.length <= MaxExprSizeInBytes,
      (),
      s"Script is too large: ${bs.length} bytes > $MaxExprSizeInBytes bytes"
    )

  @VisibleForTesting
  def apply(x: EXPR): Either[String, Script] = apply(V1, x)

  def apply(version: StdLibVersion, x: EXPR, checkSize: Boolean = true): Either[String, Script] =
    ExprScriptImpl(version, x)
      .asRight[String]
      .flatTap(s => if (checkSize) validateBytes(s.bytes().arr) else Right(()))

  def estimate(
    expr:      EXPR,
    version:   StdLibVersion,
    estimator: ScriptEstimator
  ): Either[String, Long] =
    for {
      scriptComplexity <- estimator(varNames(version, Expression), functionCosts(version), expr)
      _ <- Either.cond(
        scriptComplexity <= MaxComplexityByVersion(version),
        (),
        s"Script is too complex: $scriptComplexity > ${MaxComplexityByVersion(version)}"
      )
    } yield scriptComplexity

  private case class ExprScriptImpl(stdLibVersion: StdLibVersion, expr: EXPR) extends ExprScript {
    override type Expr = EXPR
    override val bytes: Coeval[ByteStr]           = Coeval.evalOnce(ByteStr(Global.serializeExpression(expr, stdLibVersion)))
    override val containsBlockV2: Coeval[Boolean] = Coeval.evalOnce(com.wavesplatform.lang.v1.compiler.сontainsBlockV2(expr))
  }

}

trait ExprScript extends Script {
  override type Expr = EXPR
  val stdLibVersion: StdLibVersion
  val expr: EXPR
}
