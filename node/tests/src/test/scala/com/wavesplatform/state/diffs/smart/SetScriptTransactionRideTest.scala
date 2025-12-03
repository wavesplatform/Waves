package com.wavesplatform.state.diffs.smart

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures.ConsensusImprovements
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.test.{NumericExt, PropSpec, produce}
import com.wavesplatform.transaction.TxHelpers.{defaultSigner, secondSigner, setScript}
import com.wavesplatform.transaction.TxVersion

class SetScriptTransactionRideTest extends PropSpec with WithDomain {
  property(s"correct mapping SetScriptTransaction with > 32 KB script from $ConsensusImprovements") {
    val dAppVerifier = TestCompiler(V6).compileContract(
      s"""
         | @Verifier(tx)
         | func verify() = {
         |   match tx {
         |     case ss: SetScriptTransaction => ss.script.value().size() == 32779
         |     case _                        => throw()
         |   }
         | }
       """.stripMargin
    )
    val exprVerifier = TestCompiler(V6).compileExpression(
      s"""
         | match tx {
         |   case ss: SetScriptTransaction => ss.script.value().size() == 32779
         |   case _                        => throw()
         | }
       """.stripMargin
    )
    val dApp = TestCompiler(V6).compileContract(
      s"""
         | let a = ${Seq.fill(2519)("sigVerify(base58'', base58'', base58'')").mkString(" && ")}
         | @Callable(i)
         | func default() = []
       """.stripMargin
    )
    withDomain(
      DomainPresets.RideV6.setFeaturesHeight(ConsensusImprovements -> 4),
      AddrWithBalance.enoughBalances(secondSigner)
    ) { d =>
      d.appendBlock(setScript(defaultSigner, dAppVerifier), setScript(secondSigner, exprVerifier))
      d.appendBlockE(setScript(defaultSigner, dApp, 1.waves, TxVersion.V2)) should produce(
        "value() called on unit value while accessing field 'script'"
      )
      d.appendBlockE(setScript(secondSigner, dApp, 1.waves, TxVersion.V2)) should produce(
        "value() called on unit value while accessing field 'script'"
      )
      d.appendBlock()
      d.appendAndAssertSucceed(setScript(defaultSigner, dApp, 1.waves, TxVersion.V2))
      d.appendAndAssertSucceed(setScript(secondSigner, dApp, 1.waves, TxVersion.V2))
    }
  }
}
