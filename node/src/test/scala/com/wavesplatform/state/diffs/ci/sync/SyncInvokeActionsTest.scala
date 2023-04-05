package com.wavesplatform.state.diffs.ci.sync

import com.wavesplatform.TestValues.invokeFee
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.*

class SyncInvokeActionsTest extends PropSpec with WithDomain {
  import DomainPresets.*

  private val dApp1Signer  = secondSigner
  private val dApp1Address = secondAddress
  private val dApp2Signer  = signer(2)
  private val dApp2Address = signer(2).toAddress

  property("can't reissue asset issued by other dApp in the chain") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict assetId = Address(base58'$dApp2Address').invoke("default", [], [])
           |   [
           |     Reissue(assetId.exactAs[ByteVector], 10, true)
           |   ]
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   let issue = Issue("name", "", 1000, 4, true, unit, 0)
           |   ([issue], issue.calculateAssetId())
           | }
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendAndAssertFailed(invoke(dApp1Address, fee = invokeFee(issues = 1)), "Asset was issued by other address")
    }
  }

  property("can transfer from parent dApp asset issued in the chain") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict assetId = Address(base58'$dApp2Address').invoke("default", [], [])
           |   [
           |     ScriptTransfer(i.caller, 700, assetId.exactAs[ByteVector])
           |   ]
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   let issue   = Issue("name", "", 1000, 4, true, unit, 0)
           |   let assetId = issue.calculateAssetId()
           |   ([issue, ScriptTransfer(i.caller, 900, assetId)], assetId)
           | }
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendAndAssertSucceed(invoke(dApp1Address, fee = invokeFee(issues = 1)))
      d.liquidSnapshot.balances.get((dApp1Address, Waves)) shouldBe empty
      d.liquidSnapshot.balances.get((dApp2Address, Waves)) shouldBe empty
      d.liquidSnapshot.balances.collect { case ((address, asset), amount) if address == defaultAddress && asset != Waves => amount}.head shouldBe 1000
    }
  }

  property("can't attach asset that will be issued later in the chain") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   let issue   = Issue("name", "", 1000, 4, true, unit, 0)
           |   let assetId = issue.calculateAssetId()
           |   strict r = Address(base58'$dApp2Address').invoke("default", [], [AttachedPayment(assetId, 1)])
           |   [ issue ]
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   let assetId = i.payments[0].assetId
           |   let amount  = i.payments[0].amount
           |   ([ScriptTransfer(i.caller, amount, assetId)], assetId)
           | }
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendBlockE(invoke(dApp1Address, fee = invokeFee(issues = 1))) should produce("is not found on the blockchain")
    }
  }

  property("can't transfer asset that will be issued later from the same dApp") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp = TestCompiler(V5).compileContract(
        s"""
           | let issue = Issue("name", "", 1000, 4, true, unit, 0)
           |
           | @Callable(i)
           | func default() = {
           |   strict r = this.invoke("transfer", [], [])
           |   [issue]
           | }
           |
           | @Callable(i)
           | func transfer() =
           |   [ScriptTransfer(i.originCaller, 1, issue.calculateAssetId())]
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp))
      d.appendBlockE(invoke(dApp1Address, fee = invokeFee(issues = 1))) should produce("is not found on the blockchain")
    }
  }

  property("can't issue the same asset in two dApps in the chain") {
    withDomain(RideV5, AddrWithBalance.enoughBalances(dApp1Signer, dApp2Signer)) { d =>
      val dApp1 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   strict r = Address(base58'$dApp2Address').invoke("default", [], [])
           |   [ Issue("name", "", 1000, 4, true, unit, 0) ]
           | }
         """.stripMargin
      )
      val dApp2 = TestCompiler(V5).compileContract(
        s"""
           | @Callable(i)
           | func default() = {
           |   [ Issue("name", "", 1000, 4, true, unit, 0) ]
           | }
         """.stripMargin
      )
      d.appendBlock(setScript(dApp1Signer, dApp1), setScript(dApp2Signer, dApp2))
      d.appendAndAssertFailed(invoke(dApp1Address, fee = invokeFee(issues = 2)), "is already issued")
    }
  }
}
