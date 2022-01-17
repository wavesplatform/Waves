package com.wavesplatform.lang.v1.compiler
import com.wavesplatform.common.utils.*
import com.wavesplatform.lang.v1.compiler.CompilationError.{GenericFunctionNotFound, TypeCastAllowedOnlyForGenericList}
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler.CompilationStepResultExpr
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.*
import com.wavesplatform.lang.v1.parser.Expressions.Pos

object TypeCast {
  val ExactAs = "exactAs"
  val As      = "as"

  def apply(p: Pos, name: String, expr: CompilationStepResultExpr, targetType: FINAL, provideRuntimeTypeOnError: Boolean): CompilationStepResultExpr =
    name match {
      case ExactAs => exactTo(p, targetType, expr, provideRuntimeTypeOnError)
      case As      => to(p, targetType, expr)
      case other   => expr.copy(errors = Seq(GenericFunctionNotFound(p.start, p.end, other)))
    }

  private def exactTo(p: Pos, targetType: FINAL, expr: CompilationStepResultExpr, provideRuntimeTypeOnError: Boolean): CompilationStepResultExpr = {
    val toMessage = if (provideRuntimeTypeOnError) runtimeErrorMessage _ else compileTimeErrorMessage _
    cast(
      p,
      targetType,
      targetType,
      expr,
      FUNCTION_CALL(throwWithMessage, List(toMessage(targetType, expr)))
    )
  }

  private def runtimeErrorMessage(targetType: FINAL, expr: CompilationStepResultExpr): FUNCTION_CALL =
    FUNCTION_CALL(
      sumString.header,
      List(
        FUNCTION_CALL(_getType.header, List(expr.expr)),
        CONST_STRING(s" couldn't be cast to $targetType").explicitGet()
      )
    )

  private def compileTimeErrorMessage(targetType: FINAL, expr: CompilationStepResultExpr): CONST_STRING =
    CONST_STRING(s"Couldn't cast ${expr.t} to $targetType").explicitGet()

  private def to(p: Pos, targetType: FINAL, expr: CompilationStepResultExpr): CompilationStepResultExpr =
    cast(p, targetType, UNION(targetType, UNIT), expr, REF(unitVarName))

  private def cast(p: Pos, targetType: FINAL, resultExprType: FINAL, expr: CompilationStepResultExpr, onError: EXPR): CompilationStepResultExpr = {
    targetType match {
      case LIST(t) if t != ANY =>
        expr.copy(errors = Seq(TypeCastAllowedOnlyForGenericList(p.start, p.end)))
      case _ =>
        val r =
          BLOCK(
            LET("@", expr.expr),
            IF(
              FUNCTION_CALL(_isInstanceOf.header, List(REF("@"), CONST_STRING(targetType.name).explicitGet())),
              REF("@"),
              onError
            )
          )
        expr.copy(t = resultExprType, expr = r)
    }
  }
}
