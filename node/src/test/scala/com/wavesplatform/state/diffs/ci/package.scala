package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.contract.DApp
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, Expression, ScriptType, StdLibVersion, V3, DApp => DAppType}
import com.wavesplatform.lang.v1.compiler.{ContractCompiler, ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Expressions.{DAPP, EXPR}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.state.diffs.FeeValidation._
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import org.scalacheck.Gen

package object ci {
  private val invokeFee = FeeUnit * FeeConstants(InvokeScriptTransaction.typeId)

  def ciFee(sc: Int = 0, nonNftIssue: Int = 0, dApps: Int = 0): Gen[Long] =
    Gen.choose(
      invokeFee * (dApps + 1) + sc * ScriptExtraFee + nonNftIssue * FeeConstants(IssueTransaction.typeId) * FeeUnit,
      invokeFee * (dApps + 1) + (sc + 1) * ScriptExtraFee - 1 + nonNftIssue * FeeConstants(IssueTransaction.typeId) * FeeUnit
    )

  def compileContractFromExpr(expr: DAPP, version: StdLibVersion = V3): DApp = {
    val ctx =
      PureContext.build(version).withEnvironment[Environment] |+|
        CryptoContext.build(Global, version).withEnvironment[Environment] |+|
        WavesContext.build(
          Global,
          DirectiveSet(version, Account, DAppType).explicitGet()
        )
    ContractCompiler(ctx.compilerContext, expr, version).explicitGet()
  }

  def compileExpr(expr: EXPR, version: StdLibVersion, scriptType: ScriptType): Terms.EXPR = {
    val ctx =
      PureContext.build(version).withEnvironment[Environment] |+|
        CryptoContext.build(Global, version).withEnvironment[Environment] |+|
        WavesContext.build(
          Global,
          DirectiveSet(version, scriptType, Expression).explicitGet()
        )
    ExpressionCompiler(ctx.compilerContext, expr).explicitGet()._1
  }
}
