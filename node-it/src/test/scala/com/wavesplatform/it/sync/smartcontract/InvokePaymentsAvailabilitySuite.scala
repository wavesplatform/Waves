package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.StdLibVersion.V5
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class InvokePaymentsAvailabilitySuite extends BaseTransactionSuite {

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(
        _.preactivatedFeatures(
          (BlockchainFeatures.Ride4DApps.id, 0),
          (BlockchainFeatures.BlockV5.id, 0),
          (BlockchainFeatures.SynchronousCalls.id, 0)
        )
      )
      .withDefault(1)
      .buildNonConflicting()

  private lazy val (caller, callerAddress)           = (firstKeyPair, firstAddress)
  private lazy val (callingDApp, callingDAppAddress) = (secondKeyPair, secondAddress)
  private lazy val (proxyDApp, proxyDAppAddress)     = (thirdKeyPair, thirdAddress)

  private def syncDApp(dApp: String) =
    ScriptCompiler(
      s"""
       |{-# STDLIB_VERSION 5 #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |
       | let dApp2 = Address(base58'$dApp')
       |
       | @Callable(inv)
       | func default() = {
       |    let pmt = inv.payments[0]
       |    strict invokeV4 = dApp2.invoke("stake", nil, [AttachedPayment(pmt.assetId, pmt.amount)])
       |    [
       |       IntegerEntry("balance_self", this.assetBalance(pmt.assetId.value())),
       |       IntegerEntry("balance_calling_dApp", dApp2.assetBalance(pmt.assetId.value()))
       |    ]
       | }
       |
         """.stripMargin,
      isAssetScript = false,
      ScriptEstimatorV3
    ).explicitGet()._1.bytes().base64

  private def dApp(version: StdLibVersion) = {
    val data =
      if (version > V3)
        s"""
           |let pmtAssetId = inv.payments[0].assetId.value()
           |[
           |  IntegerEntry("balance_self", this.assetBalance(pmtAssetId)),
           |  IntegerEntry("balance_caller", inv.caller.assetBalance(pmtAssetId))
           |]
         """.stripMargin
      else
        s"""
           |let pmtAssetId = inv.payment.value().assetId.value()
           |WriteSet([
           |  DataEntry("balance_self", this.assetBalance(pmtAssetId)),
           |  DataEntry("balance_caller", inv.caller.assetBalance(pmtAssetId))
           |])
         """.stripMargin

    ScriptCompiler(
      s"""
       | {-# STDLIB_VERSION ${version.id}  #-}
       | {-# CONTENT_TYPE   DAPP           #-}
       | {-# SCRIPT_TYPE    ACCOUNT        #-}
       |
       | @Callable(inv)
       | func stake() = {
       |   $data
       | }
     """.stripMargin,
      isAssetScript = false,
      ScriptEstimatorV3
    ).explicitGet()._1.bytes().base64
  }

  private val paymentAmount = 12345
  private val issueAmount   = 1000 * 1000

  test("payments availability") {
    val assetId = sender.issue(caller, quantity = issueAmount, waitForTx = true).id
    val asset   = IssuedAsset(ByteStr.decodeBase58(assetId).get)
    sender.setScript(proxyDApp, Some(syncDApp(callingDAppAddress)), waitForTx = true)

    DirectiveDictionary[StdLibVersion].all
      .filter(_ >= V3)
      .foreach { v =>
        sender.setScript(callingDApp, Some(dApp(v)), waitForTx = true)

        val callerStartBalance      = sender.assetBalance(callerAddress, assetId).balance
        val proxyStartBalance       = sender.assetBalance(proxyDAppAddress, assetId).balance
        val callingDAppStartBalance = sender.assetBalance(callingDAppAddress, assetId).balance

        sender.invokeScript(caller, proxyDAppAddress, payment = Seq(Payment(paymentAmount, asset)), fee = invokeFee, waitForTx = true)
        sender.assetBalance(callerAddress, assetId).balance shouldBe callerStartBalance - paymentAmount

        val (proxyDAppBalance, callingDAppBalance) = if (v >= V5) (0, paymentAmount) else (paymentAmount, 0)
        List(
          sender.assetBalance(proxyDAppAddress, assetId).balance,
          sender.getData(proxyDAppAddress, "balance_self").head.value,
          sender.getData(callingDAppAddress, "balance_caller").head.value
        ).foreach(_ shouldBe proxyStartBalance + proxyDAppBalance)
        List(
          sender.assetBalance(callingDAppAddress, assetId).balance,
          sender.getData(proxyDAppAddress, "balance_calling_dApp").head.value,
          sender.getData(callingDAppAddress, "balance_self").head.value
        ).foreach(_ shouldBe callingDAppStartBalance + callingDAppBalance)
      }
  }
}
