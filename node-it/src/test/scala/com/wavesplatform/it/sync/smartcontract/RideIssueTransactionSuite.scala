package com.wavesplatform.it.sync.smartcontract

import java.nio.charset.StandardCharsets

import com.typesafe.config.Config
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class RideIssueTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .buildNonConflicting()

  val assetName        = "Asset name"
  val assetDescription = "Asset description"
  val assetQuantity    = 2000

  val issueCheckV4 =
    compile(
      s"""
         | {-# STDLIB_VERSION 4 #-}
         | {-# CONTENT_TYPE EXPRESSION #-}
         | {-# SCRIPT_TYPE ACCOUNT #-}
         |
         | match tx {
         |   case i: IssueTransaction =>
         |     i.name        == "$assetName"         &&
         |     i.description == "$assetDescription"
         |
         |   case _ =>
         |     throw("unexpected")
         | }
         |
          """.stripMargin
    )

  val issueCheckV3 =
    compile(
      s"""
         | {-# STDLIB_VERSION 3 #-}
         | {-# CONTENT_TYPE EXPRESSION #-}
         | {-# SCRIPT_TYPE ACCOUNT #-}
         |
         | match tx {
         |   case i: IssueTransaction =>
         |     i.name        == base64'${ByteStr(assetName.getBytes(StandardCharsets.UTF_8)).base64}'        &&
         |     i.description == base64'${ByteStr(assetDescription.getBytes(StandardCharsets.UTF_8)).base64}'
         |
         |   case _ =>
         |     throw("unexpected")
         | }
         |
          """.stripMargin
    )

  def compile(script: String) =
    ScriptCompiler.compile(script, ScriptEstimatorV3).explicitGet()._1.bytes().base64

  test("check issuing asset name and description using V3 and V4 script") {
    assertSuccessIssue(firstAddress, issueCheckV3)
    assertSuccessIssue(secondAddress, issueCheckV4)
  }

  def assertSuccessIssue(address: String, script: String) = {
    val setScriptId = sender.setScript(address, Some(script), setScriptFee, waitForTx = true).id

    val scriptInfo = sender.addressScriptInfo(address)
    scriptInfo.script.isEmpty shouldBe false
    scriptInfo.scriptText.isEmpty shouldBe false
    scriptInfo.script.get.startsWith("base64:") shouldBe true

    sender.transactionInfo[TransactionInfo](setScriptId).script.get.startsWith("base64:") shouldBe true

    val assetId = sender.issue(address, assetName, assetDescription, assetQuantity, fee = issueFee + smartFee, waitForTx = true).id

    sender.assertAssetBalance(address, assetId, assetQuantity)

    val asset = sender.assetsDetails(assetId)
    asset.name shouldBe assetName
    asset.description shouldBe assetDescription
  }
}
