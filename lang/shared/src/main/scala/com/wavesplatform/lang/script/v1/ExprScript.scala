package com.wavesplatform.lang.script.v1

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.ContractLimits._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.{BaseGlobal, ScriptEstimator}
import monix.eval.Coeval

object ExprScript {

  private val Global: BaseGlobal = com.wavesplatform.lang.Global // Hack for IDEA

  val checksumLength = 4

  def validateBytes(bs: Array[Byte]): Either[String, Unit] =
    Either.cond(bs.length <= MaxExprSizeInBytes, (), s"Script is too large: ${bs.length} bytes > $MaxExprSizeInBytes bytes")

  def apply(x: EXPR): Either[String, Script] = apply(V1, x)

  def apply(version: StdLibVersion, x: EXPR, checkSize: Boolean = true, checkComplexity: Boolean = true): Either[String, Script] =
    for {
      scriptComplexity <- ScriptEstimator(varNames(version, Expression), functionCosts(version), x)
      _ <- Either.cond(!checkComplexity || scriptComplexity <= MaxComplexityByVersion(version),
                       (),
                       s"Script is too complex: $scriptComplexity > ${MaxComplexityByVersion(version)}")
      s = new ExprScriptImpl(version, x, scriptComplexity)
      _ <- if (checkSize) validateBytes(s.bytes().arr) else Right(())
    } yield s

  private case class ExprScriptImpl(stdLibVersion: StdLibVersion, expr: EXPR, complexity: Long) extends ExprScript {
    override val complexityMap: Map[String, Long] = Map.empty
    override type Expr = EXPR
    override val bytes: Coeval[ByteStr]           = Coeval.evalOnce(ByteStr(Global.serializeExpression(expr, stdLibVersion)))
    override val containsBlockV2: Coeval[Boolean] = Coeval.evalOnce(com.wavesplatform.lang.v1.compiler.сontainsBlockV2(expr))
  }

}

trait ExprScript extends Script {
  override type Expr = EXPR
  val stdLibVersion: StdLibVersion
  val expr: EXPR
  val complexity: Long
}
