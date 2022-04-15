package com.wavesplatform.state.diffs.ci

import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers.{invoke, issue, secondSigner, setScript}

class ScriptActionsTest extends PropSpec with WithDomain {
  import DomainPresets._

  property("set reissuable = false and try to reissue via invoke") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val issueTx = issue(secondSigner)
      val asset   = IssuedAsset(issueTx.id())
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() =
           |   [
           |     Reissue(base58'$asset', 1, false)
           |   ]
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp), issueTx)
      d.appendAndAssertSucceed(invoke())
      d.appendAndAssertFailed(invoke(), "Asset is not reissuable")
    }
  }
}
