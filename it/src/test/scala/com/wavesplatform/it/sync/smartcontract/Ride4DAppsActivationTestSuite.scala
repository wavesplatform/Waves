package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.sync.{issueFee, minFee, setScriptFee, smartFee, smartMinFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.{IssueTransactionV2, SetAssetScriptTransaction}
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.transfer.TransferTransactionV2
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsNumber

import scala.concurrent.duration._

class Ride4DAppsActivationTestSuite extends BaseTransactionSuite with CancelAfterFailure {
  import Ride4DAppsActivationTestSuite._

  override protected def nodeConfigs: Seq[Config] = configWithRide4DAppsFeature

  private val smartAcc  = pkByAddress(firstAddress)
  private val callerAcc = pkByAddress(secondAddress)

  val scriptV3 = ScriptCompiler.compile("""
                                            |{-# STDLIB_VERSION 3 #-}
                                            |{-# CONTENT_TYPE DAPP #-}
                                            |
                                            |@Callable(i)
                                            |func doAction() = { WriteSet([DataEntry("0", true)]) }
                                            |
                                            |@Verifier(i)
                                            |func verify() = { true }
                                          """.stripMargin).explicitGet()._1
  val scriptV2 = ScriptCompiler.compile("""
                                          |func isTrue() = true
                                          |isTrue()
                                        """.stripMargin).explicitGet()._1

  test("send waves to accounts") {
    val scriptTransfer =
      TransferTransactionV2
        .selfSigned(
          assetId = Waves,
          sender = sender.privateKey,
          recipient = smartAcc,
          amount = 5.waves,
          timestamp = System.currentTimeMillis(),
          feeAssetId = Waves,
          feeAmount = minFee,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
    val scriptTransferId = sender
      .signedBroadcast(scriptTransfer.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt)))
      .id

    val callerTransfer =
      TransferTransactionV2
        .selfSigned(
          assetId = Waves,
          sender = sender.privateKey,
          recipient = callerAcc,
          amount = 5.waves,
          timestamp = System.currentTimeMillis(),
          feeAssetId = Waves,
          feeAmount = minFee,
          attachment = Array.emptyByteArray
        )
        .explicitGet()
    val callerTransferId = sender
      .signedBroadcast(callerTransfer.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(callerTransferId)
    nodes.waitForTransaction(scriptTransferId)
  }

  test("can't set contract to account before Ride4DApps activation") {
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(smartAcc, Some(scriptV3), setScriptFee + smartFee, System.currentTimeMillis())
      .explicitGet()
    assertBadRequestAndMessage(
      sender
        .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt))),
      "RIDE 4 DAPPS feature has not been activated yet"
    )
  }

  test("can't set script with user function to account before Ride4DApps activation") {
    val setFuncScriptTransaction = SetScriptTransaction
      .selfSigned(smartAcc, Some(scriptV2), setScriptFee, System.currentTimeMillis())
      .explicitGet()
    assertBadRequestAndMessage(
      sender
        .signedBroadcast(setFuncScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt))),
      "RIDE 4 DAPPS feature has not been activated yet"
    )
  }

  test("can't invoke script before Ride4DApps activation") {
    val invokeScriptTransaction = InvokeScriptTransaction
      .selfSigned(callerAcc,
                  smartAcc.toAddress,
                  Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List.empty),
                  Seq.empty,
                  smartMinFee,
                  Waves,
                  System.currentTimeMillis())
      .explicitGet()
    assertBadRequestAndMessage(
      sender.signedBroadcast(invokeScriptTransaction.json() + ("type" -> JsNumber(InvokeScriptTransaction.typeId.toInt))),
      "RIDE 4 DAPPS feature has not been activated yet"
    )
  }

  test("can't issue asset with user function in script before Ride4DApps activation") {
    val issueTransaction = IssueTransactionV2
      .selfSigned(
        AddressScheme.current.chainId,
        smartAcc,
        "Test".getBytes,
        "Test asset".getBytes,
        1000,
        0,
        reissuable = true,
        Some(scriptV2),
        issueFee,
        System.currentTimeMillis()
      )
      .explicitGet()
    assertBadRequestAndMessage(
      sender.signedBroadcast(issueTransaction.json() + ("type" -> JsNumber(IssueTransactionV2.typeId.toInt))),
      "RIDE 4 DAPPS feature has not been activated yet"
    )
  }

  test("can't set script with user function to asset before Ride4DApps activation") {
    val setAssetScriptTransaction = SetAssetScriptTransaction
      .signed(AddressScheme.current.chainId,
              smartAcc,
              Asset.IssuedAsset("Test".getBytes),
              Some(scriptV2),
              issueFee,
              System.currentTimeMillis(),
              smartAcc)
      .explicitGet()
    assertBadRequestAndMessage(
      sender.signedBroadcast(setAssetScriptTransaction.json() + ("type" -> JsNumber(SetAssetScriptTransaction.typeId.toInt))),
      "RIDE 4 DAPPS feature has not been activated yet"
    )
  }

  test(s"wait height from to $activationHeight for feature activation") {
    sender.waitForHeight(activationHeight, 5.minutes)
  }

  test("can issue asset and set script with user function after Ride4DApps activation") {
    val issueTransaction = IssueTransactionV2
      .selfSigned(
        AddressScheme.current.chainId,
        smartAcc,
        "Test".getBytes,
        "Test asset".getBytes,
        1000,
        0,
        reissuable = true,
        Some(scriptV2),
        issueFee,
        System.currentTimeMillis()
      )
      .explicitGet()
    val issueTxId = sender.signedBroadcast(issueTransaction.json() + ("type" -> JsNumber(IssueTransactionV2.typeId.toInt))).id
    sender.waitForTransaction(issueTxId)

    val setAssetScriptTransaction = SetAssetScriptTransaction
      .signed(
        AddressScheme.current.chainId,
        smartAcc,
        Asset.IssuedAsset(ByteStr.decodeBase58(issueTxId).get),
        Some(scriptV2),
        issueFee,
        System.currentTimeMillis(),
        smartAcc
      )
      .explicitGet()
    val txId = sender.signedBroadcast(setAssetScriptTransaction.json() + ("type" -> JsNumber(SetAssetScriptTransaction.typeId.toInt))).id
    sender.waitForTransaction(txId)
  }

  test("can set contract and invoke script after Ride4DApps activation") {
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(smartAcc, Some(scriptV3), setScriptFee + smartFee, System.currentTimeMillis())
      .explicitGet()
    val setScriptTxId = sender.signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt))).id
    sender.waitForTransaction(setScriptTxId)

    val invokeScriptTransaction = InvokeScriptTransaction
      .selfSigned(callerAcc,
                  smartAcc.toAddress,
                  Terms.FUNCTION_CALL(FunctionHeader.User("doAction"), List.empty),
                  Seq.empty,
                  smartMinFee,
                  Waves,
                  System.currentTimeMillis())
      .explicitGet()
    val invokeTxId = sender.signedBroadcast(invokeScriptTransaction.json() + ("type" -> JsNumber(InvokeScriptTransaction.typeId.toInt))).id
    sender.waitForTransaction(invokeTxId)

    val setFuncScriptTransaction = SetScriptTransaction
      .selfSigned(smartAcc, Some(scriptV2), setScriptFee + smartFee, System.currentTimeMillis())
      .explicitGet()
    val setFuncScriptTxId = sender.signedBroadcast(setFuncScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt))).id
    sender.waitForTransaction(setFuncScriptTxId)
  }

  test("can add user function to account script after Ride4DApps activation") {
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(smartAcc, Some(scriptV2), setScriptFee + smartFee, System.currentTimeMillis())
      .explicitGet()
    val txId = sender.signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt))).id
    sender.waitForTransaction(txId)
  }
}

object Ride4DAppsActivationTestSuite {
  val activationHeight = 25

  private val configWithRide4DAppsFeature: Seq[Config] =
    Default.map(ConfigFactory.parseString(s"""
                                             | waves.blockchain.custom.functionality {
                                             |   pre-activated-features.11 = ${activationHeight - 1}
                                             |}""".stripMargin).withFallback(_))

}
