package com.wavesplatform.state.diffs.ci
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.v1.ExprScript.ExprScriptImpl
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.state.Sponsorship
import com.wavesplatform.state.diffs.FeeValidation.FeeUnit
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers.{invoke, issue, secondAddress, secondSigner, setScript, signer, sponsor, transfer}

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

  private val preTxRate = 10L
  private val fee       = 5 * FeeUnit

  // the dApp raises its fee asset's rate in a nested invoke, then restores preTxRate for the next tx
  private def sponsorDebits(fixHeight: Int, invokes: Int): Seq[Long] = {
    val invoker = signer(2)
    withDomain(RideV6.configure(_.copy(sponsoredInvokeFeeFixHeight = fixHeight)), AddrWithBalance.enoughBalances(secondSigner, invoker)) { d =>
      val issueTx = issue(secondSigner)
      val asset   = IssuedAsset(issueTx.id())
      val dApp = TestCompiler(V6).compileContract(
        s"""
           | @Callable(i)
           | func set(rate: Int) = [SponsorFee(base58'$asset', rate)]
           |
           | @Callable(i)
           | func raise() = {
           |   strict r = invoke(this, "set", [$FeeUnit], [])
           |   [SponsorFee(base58'$asset', $preTxRate)]
           | }
        """.stripMargin
      )
      d.appendBlock(
        issueTx,
        sponsor(asset, Some(preTxRate), secondSigner),
        transfer(secondSigner, invoker.toAddress, 1_000_000, asset),
        setScript(secondSigner, dApp)
      )

      (1 to invokes).map { _ =>
        val sponsorBefore = d.balance(secondAddress)
        d.appendAndAssertSucceed(invoke(secondAddress, Some("raise"), invoker = invoker, fee = fee, feeAssetId = asset))
        sponsorBefore - d.balance(secondAddress)
      }
    }
  }

  property("sponsor pays the fee at the rate in effect before the invoke, even if a nested invoke changes it") {
    sponsorDebits(fixHeight = 0, invokes = 1) shouldBe Seq(Sponsorship.toWaves(fee, preTxRate))
  }

  property("sponsor pays the fee at the rate set by nested invokes below the fix height") {
    // genesis and setup take heights 1 and 2, so the invokes land at heights 3 and 4
    sponsorDebits(fixHeight = 4, invokes = 2) shouldBe Seq(Sponsorship.toWaves(fee, FeeUnit), Sponsorship.toWaves(fee, preTxRate))
  }
}
