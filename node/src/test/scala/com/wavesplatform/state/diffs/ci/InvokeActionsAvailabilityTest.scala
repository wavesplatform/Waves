package com.wavesplatform.state.diffs.ci

import com.wavesplatform.account.Address
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{DBCacheSettings, WithDomain, WithState}
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.{NoShrink, TestTime, TransactionGen}
import org.scalamock.scalatest.MockFactory
import org.scalatest.{EitherValues, Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class InvokeActionsAvailabilityTest
    extends PropSpec
    with ScalaCheckPropertyChecks
    with Matchers
    with TransactionGen
    with NoShrink
    with Inside
    with WithState
    with DBCacheSettings
    with MockFactory
    with WithDomain
    with EitherValues {
  import DomainPresets._

  private val time = new TestTime
  private def ts   = time.getTimestamp()

  private val transferAmount       = 100
  private val issueAmount          = 200
  private val reissueAmount        = 40
  private val burnAmount           = 80
  private val leaseAmount          = 500
  private val cancelledLeaseAmount = 1234

  private def proxyDAppScript(callingDApp: Address, callingDAppVersion: StdLibVersion): Script = {
    val assetActionsCheck =
      s"""
         | let assetId = dApp2.getBinaryValue("assetId")
         | strict c3 = if (dApp2.assetBalance(assetId) == $issueAmount + $reissueAmount - $burnAmount) then true else throw("Asset actions error")
       """.stripMargin

    val leaseActionsCheck =
      s"""
         | strict c4 = if (
         |   this.wavesBalance().effective == startBalance.effective + $leaseAmount + $transferAmount       &&
         |   dApp2.wavesBalance().available == startDApp2Balance.available - $leaseAmount - $transferAmount
         | ) then true else throw("Lease actions error")
       """.stripMargin

    TestCompiler(V5).compileContract(
      s"""
         | {-# STDLIB_VERSION 5       #-}
         | {-# CONTENT_TYPE   DAPP    #-}
         | {-# SCRIPT_TYPE    ACCOUNT #-}
         |
         | let dApp2 = Address(base58'$callingDApp')
         |
         | @Callable(inv)
         | func default() = {
         |    strict startBalance = this.wavesBalance()
         |    strict startDApp2Balance = dApp2.wavesBalance()
         |    strict r = dApp2.invoke("default", nil, [])
         |    strict c1 = if (dApp2.getStringValue("key") == "value") then true else throw("Data error")
         |    strict c2 = if (this.wavesBalance().regular == startBalance.regular + $transferAmount) then true else throw("Transfer error")
         |    ${if (callingDAppVersion >= V4) assetActionsCheck else ""}
         |    ${if (callingDAppVersion >= V5) leaseActionsCheck else ""}
         |    []
         | }
       """.stripMargin
    )
  }

  private def callingDAppScript(version: StdLibVersion): Script = {
    val fromV4Data =
      s"""
         |let issue = Issue("new asset", "", $issueAmount, 8, true, unit, 0)
         |let assetId = calculateAssetId(issue)
         |let actions =
         |[
         |  StringEntry("key", "value"),
         |  BinaryEntry("assetId", assetId),
         |  ScriptTransfer(i.caller, $transferAmount, unit),
         |  issue,
         |  Reissue(assetId, $reissueAmount, true),
         |  Burn(assetId, $burnAmount)
         |]
       """.stripMargin

    val data =
      version match {
        case V1 | V2 =>
          throw new RuntimeException("Unexpected V1 or V2 for DApp")
        case V3 =>
          s"""
             |ScriptResult(
             |  WriteSet([DataEntry("key", "value")]),
             |  TransferSet([ScriptTransfer(i.caller, $transferAmount, unit)])
             |)
           """.stripMargin
        case V4 =>
          s"""
             | $fromV4Data
             | actions
           """.stripMargin
        case _ =>
          s"""
             | $fromV4Data
             | let leaseToCancel = Lease(i.caller, $cancelledLeaseAmount)
             | actions ++ [
             |    Lease(i.caller, $leaseAmount),
             |    leaseToCancel,
             |    LeaseCancel(calculateLeaseId(leaseToCancel))
             | ]
           """.stripMargin
      }

    TestCompiler(version).compileContract(
      s"""
         | {-# STDLIB_VERSION ${version.id} #-}
         | {-# CONTENT_TYPE   DAPP          #-}
         | {-# SCRIPT_TYPE    ACCOUNT       #-}
         |
         | @Callable(i)
         | func default() = {
         |   $data
         | }
       """.stripMargin
    )
  }

  private val paymentAmount = 12345

  private def scenario(version: StdLibVersion) =
    for {
      invoker     <- accountGen
      callingDApp <- accountGen
      proxyDApp   <- accountGen
      fee         <- ciFee(nonNftIssue = 1)
      gTx1     = GenesisTransaction.create(callingDApp.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx2     = GenesisTransaction.create(invoker.toAddress, ENOUGH_AMT, ts).explicitGet()
      gTx3     = GenesisTransaction.create(proxyDApp.toAddress, ENOUGH_AMT, ts).explicitGet()
      issue    = IssueTransaction.selfSigned(2.toByte, invoker, "name", "description", ENOUGH_AMT, 1, true, None, fee, ts).explicitGet()
      ssTx     = SetScriptTransaction.selfSigned(1.toByte, callingDApp, Some(callingDAppScript(version)), fee, ts).explicitGet()
      ssTx2    = SetScriptTransaction.selfSigned(1.toByte, proxyDApp, Some(proxyDAppScript(callingDApp.toAddress, version)), fee, ts).explicitGet()
      asset    = IssuedAsset(issue.id.value())
      payments = Seq(Payment(paymentAmount, asset))
      invoke   = InvokeScriptTransaction.selfSigned(TxVersion.V3, invoker, proxyDApp.toAddress, None, payments, fee, Waves, ts).explicitGet()
    } yield (Seq(gTx1, gTx2, gTx3, ssTx, ssTx2, issue), invoke, proxyDApp.toAddress, callingDApp.toAddress)

  property("actions availability in sync call") {
    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .foreach { callingDAppVersion =>
        val (preparingTxs, invoke, proxyDApp, callingDApp) = scenario(callingDAppVersion).sample.get
        withDomain(RideV5) { d =>
          d.appendBlock(preparingTxs: _*)

          val startProxyDAppBalance   = d.blockchain.balance(proxyDApp)
          val startCallingDAppBalance = d.blockchain.balance(callingDApp)

          d.appendBlock(invoke)
          d.blockchain.transactionInfo(invoke.id.value()).get._3 shouldBe true

          d.blockchain.accountData(callingDApp, "key").get.value shouldBe "value"
          d.blockchain.balance(proxyDApp) shouldBe startProxyDAppBalance + transferAmount
          d.blockchain.balance(callingDApp) shouldBe startCallingDAppBalance - transferAmount

          if (callingDAppVersion >= V4) {
            val asset = d.blockchain.accountData(callingDApp, "assetId").get.value.asInstanceOf[ByteStr]
            d.blockchain.balance(callingDApp, IssuedAsset(asset)) shouldBe issueAmount + reissueAmount - burnAmount
          }

          if (callingDAppVersion >= V5) {
            d.blockchain.effectiveBalance(callingDApp, 0) shouldBe startCallingDAppBalance - transferAmount - leaseAmount
            d.blockchain.effectiveBalance(proxyDApp, 0) shouldBe startProxyDAppBalance + transferAmount + leaseAmount
          }
        }
      }
  }
}
