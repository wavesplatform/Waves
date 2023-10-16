package com.wavesplatform.state.diffs.ci

import com.wavesplatform.TestValues.invokeFee
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.v1.ExprScript.ExprScriptImpl
import com.wavesplatform.lang.v1.compiler.Terms.TRUE
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.state.{Portfolio, StringDataEntry}
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.RideV5
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.{TransactionType, TxHelpers}

class RideV5FailRejectTest extends PropSpec with WithDomain {
  private val assetFailScript = TestCompiler(V5).compileExpression(
    s"""
       | strict c = ${(1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}
       | if (true) then throw() else true
     """.stripMargin
  )

  private def assert[A](assert: Domain => A): Unit =
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner))(assert)

  property("invoke fails by payment script") {
    assert { d =>
      val i     = issue(script = Some(assetFailScript))
      val asset = IssuedAsset(i.id())
      val dApp = TestCompiler(V5).compileContract(
        """
          | @Callable(i)
          | func default() = []
        """.stripMargin
      )
      d.appendBlock(i)
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendAndAssertFailed(
        invoke(payments = Seq(Payment(1, asset))),
        s"Transaction is not allowed by script of the asset $asset"
      )
    }
  }

  property("invoke fails by ScriptTransfer script") {
    assert { d =>
      val i     = issue(secondSigner, script = Some(assetFailScript))
      val asset = IssuedAsset(i.id())
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = [
           |   ScriptTransfer(i.caller, 1, base58'$asset')
           | ]
         """.stripMargin
      )
      d.appendBlock(i)
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendAndAssertFailed(invoke(), s"Transaction is not allowed by script of the asset $asset")
    }
  }

  property("invoke with ScriptTransfer fails by payment script") {
    assert { d =>
      val failAssetIssue = issue(script = Some(assetFailScript))
      val trueAssetIssue = issue(secondSigner, script = Some(ExprScriptImpl(V3, false, TRUE)))
      val failAsset      = IssuedAsset(failAssetIssue.id())
      val trueAsset      = IssuedAsset(trueAssetIssue.id())
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = [
           |   ScriptTransfer(i.caller, 1, base58'$trueAsset')
           | ]
         """.stripMargin
      )
      d.appendBlock(failAssetIssue, trueAssetIssue)
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendAndAssertFailed(
        invoke(payments = Seq(Payment(1, failAsset))),
        s"Transaction is not allowed by script of the asset $failAsset"
      )
    }
  }

  property("invoke with failing payment is rejected due to dApp script") {
    assert { d =>
      val failAssetIssue = issue(script = Some(assetFailScript))
      val failAsset      = IssuedAsset(failAssetIssue.id())
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = if (true) then throw() else []
         """.stripMargin
      )
      val invokeTx = invoke(payments = Seq(Payment(1, failAsset)))
      d.appendBlock(failAssetIssue)
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlockE(invokeTx) should produce("Explicit script termination")
    }
  }

  property("failed invoke doesn't affect state") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(secondSigner, signer(10))) { d =>
      val failAssetIssue = issue(script = Some(assetFailScript))
      val trueAssetIssue = issue(secondSigner, script = Some(ExprScriptImpl(V3, false, TRUE)))
      val failAsset      = IssuedAsset(failAssetIssue.id())
      val trueAsset      = IssuedAsset(trueAssetIssue.id())
      val leaseTx        = lease(secondSigner, defaultAddress)
      val dataTx         = data(secondSigner, Seq(StringDataEntry("old", "value")))
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = [
           |   StringEntry("key1", "value"),
           |   IntegerEntry("key2", 1),
           |   BooleanEntry("key3", true),
           |   BinaryEntry("key4", base58''),
           |   DeleteEntry("old"),
           |   ScriptTransfer(i.caller, 1, unit),
           |   Issue("name", "description", 1000, 4, true, unit, 0),
           |   Reissue(base58'$trueAsset', 1, true),
           |   Burn(base58'$trueAsset', 1),
           |   Lease(i.caller, 1, 1),
           |   LeaseCancel(base58'${leaseTx.id()}'),
           |   SponsorFee(base58'', 1)
           | ]
         """.stripMargin
      )
      val invokeTx = invoke(payments = Seq(Payment(1, failAsset)))
      d.appendBlock(failAssetIssue, trueAssetIssue)
      d.appendBlock(leaseTx, dataTx)
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(invokeTx)
      d.blockchain.transactionInfo(invokeTx.id()).get._1.status == Status.Succeeded shouldBe false
      d.liquidDiff.sponsorship shouldBe Map()
      d.liquidDiff.leaseState shouldBe Map()
      d.liquidDiff.issuedAssets shouldBe Map()
      d.liquidDiff.updatedAssets shouldBe Map()
      d.liquidDiff.accountData shouldBe Map()
      d.blockchain.accountData(secondAddress, "old").get.value shouldBe "value"
      d.liquidDiff.portfolios shouldBe {
        val reward              = d.blockchain.blockReward(d.blockchain.height).get
        val setScriptFee        = FeeConstants(TransactionType.SetScript) * FeeUnit
        val previousBlockReward = (0.6 * setScriptFee).toLong
        val currentBlockReward  = (0.4 * invokeFee).toLong
        val total               = reward + previousBlockReward + currentBlockReward - invokeFee
        Map(defaultAddress -> Portfolio.waves(total))
      }
    }
  }

  property("invoke is always rejected if action address is from other network") {
    assert { d =>
      Seq(true, false).foreach { above1000Complexity =>
        val c = if (above1000Complexity) (1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ") else "1"
        val dApp = TestCompiler(V5).compileContract(
          s"""
             | @Callable(i)
             | func default() = {
             |   strict c = $c
             |   [
             |     ScriptTransfer(Address(base58'3P2pTpQhGbZrJXATKr75A1uZjeTrb4PHMYf'), 1, unit)
             |   ]
             | }
             """.stripMargin
        )
        val invokeTx = invoke()
        d.appendBlock(setScript(secondSigner, dApp))
        d.appendBlockE(invokeTx) should produce("Address belongs to another network: expected: 84(T), actual: 87(W)")
      }
    }
  }

  property("invoke is rejected or failed if attached invoke address is from other network") {
    assert { d =>
      val sigVerify = s"strict c = ${(1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}"
      Seq(false, true).foreach { complex =>
        val dApp = TestCompiler(V5).compileContract(
          s"""
             | @Callable(i)
             | func default() = {
             |   ${if (complex) sigVerify else ""}
             |   strict r = Address(base58'3P2pTpQhGbZrJXATKr75A1uZjeTrb4PHMYf').invoke("bar", [], [])
             |   []
             | }
             """.stripMargin
        )
        d.appendBlock(setScript(secondSigner, dApp))
        val invokeTx = invoke()
        if (complex) {
          d.appendAndAssertFailed(invokeTx, "Address belongs to another network: expected: 84(T), actual: 87(W)")
        } else
          d.appendBlockE(invokeTx) should produce("Address belongs to another network: expected: 84(T), actual: 87(W)")
      }
    }
  }

  property("invoke is rejected from account with non-boolean verifier") {
    assert { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = []
         """.stripMargin
      )
      val verifier = TestCompiler(V5).compileExpression(
        s"""
           | strict c = ${(1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}
           | 1
         """.stripMargin
      )
      d.appendBlock(setScript(secondSigner, dApp))
      d.appendBlock(setScript(defaultSigner, verifier))
      d.appendBlockE(invoke(fee = TxHelpers.ciFee(1))) should produce("Script returned not a boolean result")
    }
  }
}
