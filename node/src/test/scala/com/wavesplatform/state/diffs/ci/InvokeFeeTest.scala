package com.wavesplatform.state.diffs.ci
import com.wavesplatform.TestValues.invokeFee
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.FeeValidation.FeeUnit
import com.wavesplatform.state.diffs.produce
import com.wavesplatform.test.{NumericExt, PropSpec}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers._

class InvokeFeeTest extends PropSpec with WithDomain {
  import DomainPresets._

  property("invoke standard fee") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() = []
        """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(invoke(fee = invokeFee))
      d.appendBlockE(invoke(fee = invokeFee - 1)) should produce(
        "Fee in WAVES for InvokeScriptTransaction (499999 in WAVES) does not exceed minimal value of 500000 WAVES"
      )
    }
  }

  property("invoke sponsor fee") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() = []
        """.stripMargin
      )
      val issueTx   = issue()
      val asset     = IssuedAsset(issueTx.id())
      val sponsorTx = sponsor(asset, Some(FeeUnit))
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(issueTx, sponsorTx)
      d.appendBlock(invoke(fee = invokeFee, feeAssetId = asset))
      d.appendBlockE(invoke(fee = invokeFee - 1, feeAssetId = asset)) should produce(
        s"Fee in $asset for InvokeScriptTransaction (499999 in $asset) does not exceed minimal value of 500000 WAVES"
      )
    }
  }

  property("invoke is rejected if fee sponsor has not enough Waves") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner) :+ AddrWithBalance(signer(9).toAddress, 2.waves)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() = []
        """.stripMargin
      )
      val issueTx   = issue(signer(9))
      val asset     = IssuedAsset(issueTx.id())
      val sponsorTx = sponsor(asset, Some(FeeUnit), signer(9))
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(issueTx, sponsorTx)
      d.appendBlockE(invoke(feeAssetId = asset)) should produce(s"negative waves balance: ${signer(9).toAddress}, old: 0, new: -$invokeFee")
    }
  }

  property("invoke Issue fee") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() = [
          |   Issue("name", "description", 1000, 4, true, unit, 0)
          | ]
        """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(invoke(fee = invokeFee(issues = 1)))
      val failed = invoke(fee = invokeFee(issues = 1) - 1)
      d.appendBlock(failed)
      d.liquidDiff.errorMessage(failed.id()).get.text should include(
        "Fee in WAVES for InvokeScriptTransaction (100499999 in WAVES) with 1 assets issued does not exceed minimal value of 100500000 WAVES"
      )
    }
  }

  property("invoke negative fee") {
    (the[Exception] thrownBy invoke(fee = -1)).getMessage should include("InsufficientFee")
  }

  property("invoke sponsor fee via non-sponsored asset") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() = []
        """.stripMargin
      )
      val issueTx = issue()
      val asset   = IssuedAsset(issueTx.id())
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(issueTx)
      d.appendBlockE(invoke(feeAssetId = asset)) should produce(s"Asset $asset is not sponsored, cannot be used to pay fees")
    }
  }
}
