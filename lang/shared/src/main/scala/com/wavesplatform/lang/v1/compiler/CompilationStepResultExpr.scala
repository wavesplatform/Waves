package com.wavesplatform.lang.v1.compiler

import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.parser.Expressions

case class CompilationStepResultExpr(
    ctx: CompilerContext,
    expr: Terms.EXPR,
    t: FINAL,
    parseNodeExpr: Expressions.EXPR,
    errors: Iterable[CompilationError] = Iterable.empty
)
