package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, ScriptType, StdLibVersion, V3, DApp => DAppType}
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Expressions.{DAPP, EXPR}
import com.wavesplatform.lang.{Common, Global}
import com.wavesplatform.state.diffs.FeeValidation._
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import org.scalacheck.Gen

package object ci {
  def ciFee(sc: Int = 0): Gen[Long] =
    Gen.choose(
      FeeUnit * FeeConstants(InvokeScriptTransaction.typeId) + sc * ScriptExtraFee,
      FeeUnit * FeeConstants(InvokeScriptTransaction.typeId) + (sc + 1) * ScriptExtraFee - 1
    )

  def compileContractFromExpr(expr: DAPP, version: StdLibVersion = V3): DApp = {
    val ctx =
      PureContext.build(Global, version)   |+|
      CryptoContext.build(Global, version) |+|
      WavesContext.build(
        DirectiveSet(version, Account, DAppType).explicitGet(),
        Common.emptyBlockchainEnvironment()
      )
    ContractCompiler(ctx.compilerContext, expr, version).explicitGet()
  }

  def compileExpr(expr: EXPR, version: StdLibVersion, scriptType: ScriptType): Terms.EXPR = {
    val ctx =
      PureContext.build(Global, version)   |+|
      CryptoContext.build(Global, version) |+|
      WavesContext.build(
        DirectiveSet(version, scriptType, Expression).explicitGet(),
        Common.emptyBlockchainEnvironment()
      )
    ExpressionCompiler(ctx.compilerContext, expr).explicitGet()._1
  }
}
