package com.wavesplatform.lang.v1.compiler
import com.wavesplatform.common.utils._
import com.wavesplatform.lang.v1.compiler.CompilationError.{GenericFunctionNotFound, TypeCastAllowedOnlyForGenericList}
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
      case ExactAs => exactTo(p, t, expr)
      case As      => to(p, t, expr)
      case other   => expr.copy(errors = Seq(GenericFunctionNotFound(p.start, p.end, other)))
    }

  private def exactTo(p: Pos, t: FINAL, expr: CompilationStepResultExpr): CompilationStepResultExpr =
    cast(
      p,
      t,
      t,
      expr,
      FUNCTION_CALL(
        throwWithMessage,
        List(
          FUNCTION_CALL(
            sumString.header,
            List(
              FUNCTION_CALL(_getType.header, List(expr.expr)),
              CONST_STRING(s" couldn't be cast to $t").explicitGet()
            )
          )
        )
      )
    )

  private def to(p: Pos, t: FINAL, expr: CompilationStepResultExpr): CompilationStepResultExpr =
    cast(p, t, UNION(t, UNIT), expr, REF(unitVarName))

  private def cast(p: Pos, expectingType: FINAL, resultType: FINAL, expr: CompilationStepResultExpr, onError: EXPR): CompilationStepResultExpr = {
    expectingType match {
      case LIST(t) if t != ANY =>
        expr.copy(errors = Seq(TypeCastAllowedOnlyForGenericList(p.start, p.end)))
      case _ =>
        val r = IF(
          FUNCTION_CALL(_isInstanceOf.header, List(expr.expr, CONST_STRING(expectingType.name).explicitGet())),
          expr.expr,
          onError
        )
        expr.copy(t = resultType, expr = r)
    }
  }
}
