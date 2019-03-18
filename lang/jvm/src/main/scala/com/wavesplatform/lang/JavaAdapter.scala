package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.lang.StdLibVersion.V2
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}

object JavaAdapter {
  private val version = V2

  lazy val ctx =
    Monoid.combineAll(
      Seq(
        CryptoContext.compilerContext(com.wavesplatform.lang.Global),
        WavesContext.build(???, null).compilerContext,
        PureContext.build(version).compilerContext
      ))

  def compile(input: String): EXPR = {
    ExpressionCompiler
      .compile(input, ctx)
      .fold(
        error => throw new IllegalArgumentException(error),
        expr => expr
      )
  }
}
