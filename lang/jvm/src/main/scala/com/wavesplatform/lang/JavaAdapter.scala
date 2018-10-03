package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.lang.v1.compiler.CompilerV1
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext

object JavaAdapter {
  lazy val compiler =
    new CompilerV1(
      Monoid.combineAll(
        Seq(
          CryptoContext.compilerContext(com.wavesplatform.lang.Global),
          WavesContext.build(null).compilerContext,
          PureContext.compilerContext
        )))

  def compile(input: String): EXPR = {
    compiler
      .compile(input, List())
      .fold(
        error => throw new IllegalArgumentException(error),
        expr => expr
      )
  }
}
