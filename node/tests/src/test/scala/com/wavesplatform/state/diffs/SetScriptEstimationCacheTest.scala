package com.wavesplatform.state.diffs

import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.ContractScript.ContractScriptImpl
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers

class SetScriptEstimationCacheTest extends PropSpec with WithDomain {
  private val dApp = TestCompiler(V5).compileContract(
    """
      | @Callable(i)
      | func f(n: Int) = {
      |   let a = n + 1
      |   [IntegerEntry("a", a * 2)]
      | }
      |
      | @Verifier(tx)
      | func verify() = sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
    """.stripMargin
  )

  // Complexities of setting the script, and the same estimated without the cache.
  private def estimates(
      settings: com.wavesplatform.settings.WavesSettings
  ): ((Map[Int, Map[String, Long]], Long), (Map[Int, Map[String, Long]], Long)) =
    withDomain(settings, AddrWithBalance.enoughBalances(TxHelpers.secondSigner)) { d =>
      val tx   = TxHelpers.setScript(TxHelpers.secondSigner, dApp)
      val info = SetScriptTransactionDiff(d.blockchain)(tx).explicitGet().accountScripts(TxHelpers.secondSigner.publicKey).get
      val ContractScriptImpl(version, contract) = dApp: @unchecked
      val callables                             = SetScriptTransactionDiff.estimate(d.blockchain, version, contract).explicitGet()
      val verifier                              = DiffsCommon.countVerifierComplexity(Some(dApp), d.blockchain, isAsset = false).explicitGet().get._2
      ((info.complexitiesByEstimator, info.verifierComplexity), (callables, verifier))
    }

  property("a script set again under other settings gets the complexities of those settings") {
    val (v5, v5Uncached)       = estimates(DomainPresets.RideV5)
    val (v6, v6Uncached)       = estimates(DomainPresets.RideV6)
    val (v5Again, v5Uncached2) = estimates(DomainPresets.RideV5)

    v5 shouldBe v5Uncached
    v6 shouldBe v6Uncached
    v5Again shouldBe v5Uncached2
    v5 should not be v6
  }
}
