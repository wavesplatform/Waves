package com.wavesplatform.state.diffs.ci
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers.{invoke, issue, secondSigner, setScript}

class InvokeReissueTest extends PropSpec with WithDomain {
  import DomainPresets._

  property("invoke fails on reissue of foreign asset") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val issueTx = issue()
      val asset   = IssuedAsset(issueTx.id())
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = [
           |   Reissue(base58'$asset', 1, true)
           | ]
        """.stripMargin
      )
      val invokeTx = invoke()
      d.appendBlock(issueTx)
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(invokeTx)
      d.liquidDiff.errorMessage(invokeTx.id()).get.text should include("Asset was issued by other address")
    }
  }
}
