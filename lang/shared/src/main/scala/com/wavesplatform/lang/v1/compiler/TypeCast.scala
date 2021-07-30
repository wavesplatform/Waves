package com.wavesplatform.lang.v1.compiler
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.v1.compiler.CompilationError.FunctionNotFound
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler.CompilationStepResultExpr
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext._
import com.wavesplatform.lang.v1.parser.Expressions.Pos

object TypeCast {
  val ExactAs = "exactAs"
  val As      = "as"

  def apply(p: Pos, name: String, expr: CompilationStepResultExpr, t: FINAL): CompilationStepResultExpr =
    name match {
      case ExactAs => exactTo(t, expr)
      case As      => to(t, expr)
      case other   => expr.copy(errors = Seq(FunctionNotFound(p.start, p.end, other, Nil)))
    }

  private def exactTo(t: FINAL, expr: CompilationStepResultExpr): CompilationStepResultExpr =
    cast(t, t, expr, FUNCTION_CALL(throwWithMessage, List(CONST_STRING(s"Type cast error").explicitGet())))

  private def to(t: FINAL, expr: CompilationStepResultExpr): CompilationStepResultExpr =
    cast(t, UNION(t, UNIT), expr, REF(unitVarName))

  private def cast(t: FINAL, resultType: FINAL, expr: CompilationStepResultExpr, onError: EXPR): CompilationStepResultExpr = {
    val r = IF(
      FUNCTION_CALL(_isInstanceOf.header, List(expr.expr, CONST_STRING(t.name).explicitGet())),
      expr.expr,
      onError
    )
    expr.copy(t = resultType, expr = r)
  }
}
