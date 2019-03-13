package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.sync.{issueFee, minFee, setScriptFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.assets.{IssueTransactionV2, SetAssetScriptTransaction}
import com.wavesplatform.transaction.smart.{ContractInvocationTransaction, SetScriptTransaction}
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

  test("prepare accounts") {
    val contractTransfer =
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
    val contractTransferId = sender
      .signedBroadcast(contractTransfer.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt)))
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
    nodes.waitForTransaction(contractTransferId)
  }

  test("can't set contract before Ride4DApps activation") {
    val script = ScriptCompiler.compile("""
        |{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE CONTRACT #-}
        |
        |@Callable(i)
        |func doAction() = { WriteSet([DataEntry("0", true)]) }
        |
        |@Verifier(i)
        |func verify() = { true }
      """.stripMargin).explicitGet()._1
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(smartAcc, Some(script), setScriptFee, System.currentTimeMillis())
      .explicitGet()
    assertBadRequestAndMessage(
      sender
        .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt))),
      "Ride4DApps has not been activated yet"
    )
  }

  test("can't add user function to account script before Ride4DApps activation") {
    val script = ScriptCompiler.compile("""
        |func isTrue() = true
        |isTrue()
      """.stripMargin).explicitGet()._1
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(smartAcc, Some(script), setScriptFee, System.currentTimeMillis())
      .explicitGet()
    assertBadRequestAndMessage(
      sender.signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt))),
      "Ride4DApps has not been activated yet"
    )
  }

  test("can't issue asset with user function in script before Ride4DApps activation") {
    val script = ScriptCompiler.compile("""
        |func isTrue() = true
        |isTrue()
      """.stripMargin).explicitGet()._1
    val issueTransaction = IssueTransactionV2
      .selfSigned(AddressScheme.current.chainId,
                  smartAcc,
                  "Test".getBytes,
                  "Test asset".getBytes,
                  1000,
                  0,
                  reissuable = true,
                  Some(script),
                  issueFee,
                  System.currentTimeMillis())
      .explicitGet()
    assertBadRequestAndMessage(
      sender.signedBroadcast(issueTransaction.json() + ("type" -> JsNumber(IssueTransactionV2.typeId.toInt))),
      "Ride4DApps has not been activated yet"
    )
  }

  test("can't reissue asset with user function in script before Ride4DApps activation") {
    val script = ScriptCompiler.compile("""
        |func isTrue() = true
        |isTrue()
      """.stripMargin).explicitGet()._1
    val issueTransaction = SetAssetScriptTransaction
      .signed(AddressScheme.current.chainId,
              smartAcc,
              Asset.IssuedAsset("Test".getBytes),
              Some(script),
              issueFee,
              System.currentTimeMillis(),
              smartAcc)
      .explicitGet()
    assertBadRequestAndMessage(
      sender.signedBroadcast(issueTransaction.json() + ("type" -> JsNumber(SetAssetScriptTransaction.typeId.toInt))),
      "Ride4DApps has not been activated yet"
    )
  }

  test("can't invoke script before Ride4DApps activation") {
    val invokeScriptTransaction = ContractInvocationTransaction
      .selfSigned(callerAcc,
                  smartAcc.toAddress,
                  Terms.FUNCTION_CALL(FunctionHeader.User("foo"), List.empty),
                  Seq.empty,
                  0.001.waves,
                  Waves,
                  System.currentTimeMillis())
      .explicitGet()
    assertBadRequestAndMessage(
      sender.signedBroadcast(invokeScriptTransaction.json() + ("type" -> JsNumber(ContractInvocationTransaction.typeId.toInt))),
      "Ride4DApps has not been activated yet"
    )
  }

  test("wait until feature activated") {
    sender.waitForHeight(activationHeight, 5.minutes)
  }

  test("can set contract after Ride4DApps activation") {}
}

object Ride4DAppsActivationTestSuite {
  val activationHeight = 25

  private val configWithRide4DAppsFeature: Seq[Config] =
    Default.map(ConfigFactory.parseString(s"""
                                             | waves.blockchain.custom.functionality {
                                             |   pre-activated-features.11 = $activationHeight
                                             |}""".stripMargin).withFallback(_))

}
