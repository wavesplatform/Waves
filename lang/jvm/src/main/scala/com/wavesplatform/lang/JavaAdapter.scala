package com.wavesplatform.lang

import cats.kernel.Monoid
import com.wavesplatform.lang.Version.ContractV
import com.wavesplatform.lang.v1.compiler.ExpressionCompilerV1
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}

object JavaAdapter {
  private val version = ContractV

  lazy val compiler =
    new ExpressionCompilerV1(
      Monoid.combineAll(Seq(
        CryptoContext.compilerContext(com.wavesplatform.lang.Global),
        WavesContext.build(version, null, false).compilerContext,
        PureContext.build(version).compilerContext
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
