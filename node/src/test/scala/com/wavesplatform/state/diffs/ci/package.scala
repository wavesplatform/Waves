package com.wavesplatform.state.diffs

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Expression, ScriptType, StdLibVersion}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.script.v1.ExprScript
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.state.diffs.FeeValidation._
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction
import org.scalacheck.Gen

package object ci {
  private val invokeFee = FeeUnit * FeeConstants(InvokeScriptTransaction.typeId)

  def ciFee(sc: Int = 0, nonNftIssue: Int = 0): Gen[Long] =
    Gen.choose(
      invokeFee + sc * ScriptExtraFee + nonNftIssue * FeeConstants(IssueTransaction.typeId) * FeeUnit,
      invokeFee + (sc + 1) * ScriptExtraFee - 1 + nonNftIssue * FeeConstants(IssueTransaction.typeId) * FeeUnit
    )

  def compileExpr(script: String, version: StdLibVersion, scriptType: ScriptType): Script =
    ExprScript(
      version,
      ExpressionCompiler(
        (PureContext.build(version).withEnvironment[Environment] |+|
          CryptoContext.build(Global, version).withEnvironment[Environment] |+|
          WavesContext.build(
            Global,
            DirectiveSet(version, scriptType, Expression).explicitGet()
          )).compilerContext,
        Parser.parseExpr(script).get.value
      ).explicitGet()._1
    ).explicitGet()
}
