package com.wavesplatform.transaction.smart.script.v1

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.{ContentType, Global}
import com.wavesplatform.lang.StdLibVersion._
import com.wavesplatform.lang.v1.ContractLimits._
import com.wavesplatform.lang.v1.ScriptEstimator
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.transaction.smart.script.Script
import com.wavesplatform.utils.{functionCosts, varNames}
import monix.eval.Coeval

object ExprScript {
  val checksumLength = 4

  def validateBytes(bs: Array[Byte]): Either[String, Unit] =
    Either.cond(bs.length <= MaxExprSizeInBytes, (), s"Script is too large: ${bs.length} bytes > $MaxExprSizeInBytes bytes")

  def apply(x: EXPR): Either[String, Script] = apply(V1, x)

  def apply(version: StdLibVersion, x: EXPR, checkSize: Boolean = true, checkComplexity: Boolean = true): Either[String, Script] =
    for {
      scriptComplexity <- ScriptEstimator(varNames(version, ContentType.Expression), functionCosts(version), x)
      _ <- Either.cond(!checkComplexity || scriptComplexity <= MaxExprComplexity,
                       (),
                       s"Script is too complex: $scriptComplexity > $MaxExprComplexity")
      s = new ExprScriptImpl(version, x, scriptComplexity)
      _ <- if (checkSize) validateBytes(s.bytes().arr) else Right(())
    } yield s

  private case class ExprScriptImpl(stdLibVersion: StdLibVersion, expr: EXPR, complexity: Long) extends ExprScript {
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
