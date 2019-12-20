package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.Asset
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

import scala.concurrent.duration._

class Ride4DAppsActivationTestSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val estimator = ScriptEstimatorV2

  import Ride4DAppsActivationTestSuite._

  override protected def nodeConfigs: Seq[Config] = configWithRide4DAppsFeature

  private val smartAcc  = pkByAddress(firstAddress)
  private val callerAcc = pkByAddress(secondAddress)

  private val scriptV3 = ScriptCompiler
    .compile(
      """{-# STDLIB_VERSION 3 #-}
        |{-# CONTENT_TYPE DAPP #-}
        |
        |@Callable(i)
        |func doAction() = { WriteSet([DataEntry("0", true)]) }
        |
        |@Verifier(i)
        |func verify() = { true }""".stripMargin,
      estimator
    )
    .explicitGet()
    ._1
    .bytes()
    .base64
  private val scriptV2 = ScriptCompiler
    .compile(
      """func isTrue() = true
        |isTrue()""".stripMargin,
      estimator
    )
    .explicitGet()
    ._1
    .bytes()
    .base64

  test("send waves to accounts") {
    sender
      .transfer(
        sender.address,
        recipient = smartAcc.stringRepr,
        assetId = None,
        amount = 5.waves,
        fee = minFee,
        waitForTx = true
      )
      .id

    sender
      .transfer(
        sender.address,
        recipient = callerAcc.stringRepr,
        assetId = None,
        amount = 5.waves,
        fee = minFee,
        waitForTx = true
      )
      .id
  }

  test("can't set contract to account before Ride4DApps activation") {
    assertBadRequestAndMessage(
      sender.setScript(smartAcc.stringRepr, Some(scriptV3), setScriptFee + smartFee),
      "RIDE 4 DAPPS feature has not been activated yet"
    )
  }

  test("can't set script with user function to account before Ride4DApps activation") {
    assertBadRequestAndMessage(sender.setScript(smartAcc.stringRepr, Some(scriptV2), setScriptFee), "RIDE 4 DAPPS feature has not been activated yet")
  }

  test("can't invoke script before Ride4DApps activation") {
    assertBadRequestAndMessage(
      sender.invokeScript(callerAcc.stringRepr, smartAcc.stringRepr, Some("foo"), List.empty, Seq.empty, smartMinFee, None),
      "RIDE 4 DAPPS feature has not been activated yet"
    )
  }

  test("can't issue asset with user function in script before Ride4DApps activation") {

    assertBadRequestAndMessage(
      sender.issue(
        smartAcc.stringRepr,
        "Test",
        "Test asset",
        1000,
        8,
        fee = issueFee,
        script = Some(scriptV2)
      ),
      "RIDE 4 DAPPS feature has not been activated yet"
    )
  }

  test("can't set script with user function to asset before Ride4DApps activation") {
    assertBadRequestAndMessage(
      sender.setAssetScript(
        Asset.IssuedAsset("Test".getBytes("UTF-8")).id.base58,
        smartAcc.stringRepr,
        issueFee,
        Some(scriptV2)
      ),
      "RIDE 4 DAPPS feature has not been activated yet"
    )
  }

  test(s"wait height from to $activationHeight for feature activation") {
    sender.waitForHeight(activationHeight, 5.minutes)
  }

  test("can issue asset and set script with user function after Ride4DApps activation") {
    val issueTxId = sender
      .issue(
        smartAcc.stringRepr,
        "Test",
        "Test asset",
        1000,
        0,
        script = Some(scriptV2),
        fee = issueFee
      )
      .id

    nodes.waitForTransaction(issueTxId)

    sender
      .setAssetScript(
        issueTxId,
        smartAcc.stringRepr,
        issueFee,
        Some(scriptV2),
        waitForTx = true
      )
      .id

  }

  test("can set contract and invoke script after Ride4DApps activation") {
    sender.setScript(smartAcc.stringRepr, Some(scriptV3), setScriptFee + smartFee, waitForTx = true).id

    sender
      .invokeScript(
        callerAcc.stringRepr,
        smartAcc.stringRepr,
        Some("doAction"),
        List.empty,
        Seq.empty,
        smartMinFee,
        None,
        waitForTx = true
      )
      ._1
      .id

    sender.setScript(smartAcc.stringRepr, Some(scriptV2), setScriptFee + smartFee, waitForTx = true).id
  }

  test("can add user function to account script after Ride4DApps activation") {
    sender.setScript(smartAcc.stringRepr, Some(scriptV2), setScriptFee + smartFee, waitForTx = true).id
  }
}

object Ride4DAppsActivationTestSuite {
  val activationHeight = 15

  val configWithRide4DAppsFeature = NodeConfigs.newBuilder
    .withDefault(1)
    .withSpecial(1, _.nonMiner)
    .buildNonConflicting()
    .map(
      ConfigFactory
        .parseString(
          s"""waves.blockchain.custom.functionality {
             |  pre-activated-features.11 = ${activationHeight - 1}
             |}""".stripMargin
        )
        .withFallback(_)
    )

}
