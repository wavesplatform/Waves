package com.wavesplatform.state.diffs.ci
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.v1.ExprScript.ExprScriptImpl
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers.{invoke, issue, secondSigner, setScript}

class InvokeSponsorFeeTest extends PropSpec with WithDomain {
  import DomainPresets.*

  property("invoke fails on sponsorship of foreign asset") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val issueTx = issue()
      val asset   = IssuedAsset(issueTx.id())
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = [
           |   SponsorFee(base58'$asset', 1)
           | ]
        """.stripMargin
      )
      d.appendBlock(issueTx)
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendAndAssertFailed(invoke(), s"SponsorFee assetId=$asset was not issued from address of current dApp")
    }
  }

  property("invoke fails on sponsorship of smart asset") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val issueTx = issue(secondSigner, script = Some(ExprScriptImpl(V3, false, Terms.TRUE)))
      val asset   = IssuedAsset(issueTx.id())
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = [
           |   SponsorFee(base58'$asset', 1)
           | ]
        """.stripMargin
      )
      d.appendBlock(issueTx)
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendAndAssertFailed(invoke(), s"Sponsorship smart assets is disabled")
    }
  }
}
