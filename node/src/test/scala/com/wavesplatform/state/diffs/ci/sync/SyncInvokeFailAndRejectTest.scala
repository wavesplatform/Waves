package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.TestValues.invokeFee
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.script.v1.ExprScript.ExprScriptImpl
import com.wavesplatform.lang.v1.compiler.Terms.TRUE
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.TxMeta.Status
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.state.{Portfolio, StringDataEntry}
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TransactionType
import com.wavesplatform.transaction.TxHelpers.*
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment

class SyncInvokeFailAndRejectTest extends PropSpec with WithDomain {
  import DomainPresets.*

  private val dApp1Signer  = secondSigner
  private val dApp1Address = secondAddress
  private val dApp2Signer  = signer(2)
  private val dApp2Address = signer(2).toAddress

  private val assetFailScript = TestCompiler(V5).compileExpression(
    s"""
       | strict c = ${(1 to 5).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}
       | if (true) then throw() else true
     """.stripMargin
  )

  property("failed sync invoke doesn't affect state") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val failAssetIssue     = issue(script = Some(assetFailScript))
      val trueAssetIssue     = issue(dApp2Signer, script = Some(ExprScriptImpl(V3, false, TRUE)))
      val noScriptAssetIssue = issue(dApp2Signer)
      val failAsset          = IssuedAsset(failAssetIssue.id())
      val trueAsset          = IssuedAsset(trueAssetIssue.id())
      val noScriptAsset      = IssuedAsset(noScriptAssetIssue.id())
      val leaseTx            = lease(dApp2Signer, defaultAddress)
      val dataTx             = data(dApp2Signer, Seq(StringDataEntry("old", "value")))
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("default", [], [])
           |   []
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
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
           |   SponsorFee(base58'$noScriptAsset', 1)
           | ]
         """.stripMargin
      )
      val invokeTx = invoke(payments = Seq(Payment(1, failAsset)))
      d.appendBlock(failAssetIssue, trueAssetIssue, noScriptAssetIssue)
      d.appendBlock(leaseTx, dataTx)
      d.appendBlock(setScript(dApp1Signer, dApp1))
      d.appendBlock(setScript(dApp2Signer, dApp2))
      d.appendBlock(invokeTx)
      d.blockchain.transactionInfo(invokeTx.id()).get._1.status == Status.Succeeded shouldBe false
      d.liquidDiff.sponsorship shouldBe Map()
      d.liquidDiff.leaseState shouldBe Map()
      d.liquidDiff.issuedAssets shouldBe Map()
      d.liquidDiff.updatedAssets shouldBe Map()
      d.liquidDiff.accountData shouldBe Map()
      d.blockchain.accountData(dApp2Address, "old").get.value shouldBe "value"
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

  property("sync invoke is rejected if insufficient fee is transferred at the end") {
    val invoker = signer(10)
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("default", [], [])
           |   []
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = [
           |   ScriptTransfer(i.originCaller, $invokeFee, unit)
           | ]
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendBlockE(invoke(dApp1Address, invoker = invoker)) should produce(s"negative waves balance: ${invoker.toAddress}, old: 0, new: -500000")
    }
  }
}
