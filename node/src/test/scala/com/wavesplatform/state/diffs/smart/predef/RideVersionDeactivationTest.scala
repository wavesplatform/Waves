package com.wavesplatform.state.diffs.smart.predef

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.DomainPresets.*
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers.*

class RideVersionDeactivationTest extends PropSpec with WithDomain {
  private def dApp =
    TestCompiler(V3).compileContract(
      """
        | @Callable(i)
        | func default() = WriteSet([])
      """.stripMargin
    )

  private def verifier(version: StdLibVersion) =
    TestCompiler(version).compileExpression("true")

  property(s"RIDE V1, V2, V3 are forbidden after TransactionStateSnapshot") {
    withDomain(
      TransactionStateSnapshot.setFeaturesHeight(BlockchainFeatures.LightNode -> 4),
      AddrWithBalance.enoughBalances(defaultSigner)
    ) { d =>
      val issueTx = issue(script = Some(verifier(V7)))
      val assetId = IssuedAsset(issueTx.id())
      d.appendBlock(issueTx)
      def txs = Seq(
        setScript(defaultSigner, dApp),
        setScript(defaultSigner, verifier(V1)),
        setScript(defaultSigner, verifier(V2)),
        setScript(defaultSigner, verifier(V3)),
        setAssetScript(defaultSigner, assetId, verifier(V1)),
        setAssetScript(defaultSigner, assetId, verifier(V2)),
        setAssetScript(defaultSigner, assetId, verifier(V3)),
        issue(script = Some(verifier(V1))),
        issue(script = Some(verifier(V2))),
        issue(script = Some(verifier(V3)))
      )
      d.appendAndAssertSucceed(txs*)
      txs.foreach(d.appendBlockE(_) should produce("Script version below V4 is not allowed"))
    }
  }
}
