package com.wavesplatform.lang.v1.compiler
import com.wavesplatform.common.utils.*
import com.wavesplatform.lang.v1.compiler.CompilationError.{GenericFunctionNotFound, TypeCastAllowedOnlyForGenericList}
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.*
import com.wavesplatform.lang.v1.evaluator.ctx.impl.GlobalValNames
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.*
import com.wavesplatform.lang.v1.parser.Expressions.Pos
import com.wavesplatform.lang.v1.parser.Parser.GenericMethod.{As, ExactAs}

object TypeCast {

  def apply(p: Pos, name: String, expr: CompilationStepResultExpr, targetType: FINAL, provideRuntimeTypeOnError: Boolean): CompilationStepResultExpr =
    name match {
      case ExactAs => exactAs(p, targetType, expr, provideRuntimeTypeOnError)
      case As      => as(p, targetType, expr)
      case other   => expr.copy(errors = Seq(GenericFunctionNotFound(p.start, p.end, other)))
    }

  private def exactAs(p: Pos, targetType: FINAL, expr: CompilationStepResultExpr, provideRuntimeTypeOnError: Boolean): CompilationStepResultExpr = {
    val message = if (provideRuntimeTypeOnError) runtimeErrorMessage(targetType) else compileTimeErrorMessage(targetType, expr)
    cast(
      p,
      targetType,
      targetType,
      expr,
      FUNCTION_CALL(throwWithMessage, List(message))
    )
  }

  private def runtimeErrorMessage(targetType: FINAL): FUNCTION_CALL =
    FUNCTION_CALL(
      sumString.header,
      List(
        FUNCTION_CALL(_getType.header, List(REF("@"))),
        CONST_STRING(s" couldn't be cast to $targetType").explicitGet()
      )
    )

  private def compileTimeErrorMessage(targetType: FINAL, expr: CompilationStepResultExpr): CONST_STRING =
    CONST_STRING(s"Couldn't cast ${expr.t} to $targetType").explicitGet()

  private def as(p: Pos, targetType: FINAL, expr: CompilationStepResultExpr): CompilationStepResultExpr =
    cast(p, targetType, UNION(targetType, UNIT), expr, REF(GlobalValNames.Unit))

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
