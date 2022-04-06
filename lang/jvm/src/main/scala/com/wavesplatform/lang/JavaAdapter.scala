package com.wavesplatform.lang

import cats.syntax.semigroup.*
import com.wavesplatform.lang.directives.values.V2
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompiler}
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}

object JavaAdapter {
  private val version = V2

  private val ctx: CompilerContext =
    CryptoContext.compilerContext(Global, version) |+|
      WavesContext.build(Global, ???).compilerContext |+|
      PureContext.build(version, useNewPowPrecision = true).compilerContext

  def compile(input: String): EXPR = {
    ExpressionCompiler
      .compileBoolean(input, ctx)
      .fold(
        error => throw new IllegalArgumentException(error),
        res => res
      )
  }
}
