package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.state.BinaryDataEntry
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.{CancelAfterFailure, Matchers, OptionValues}

class InvokeCalcIssueSuite extends BaseTransactionSuite with Matchers with CancelAfterFailure with OptionValues {
  import InvokeCalcIssueSuite._

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.MultiPaymentInvokeScript.id, 0), (BlockchainFeatures.BlockV5.id, 0)))
      .buildNonConflicting()

  private val smartAcc  = firstAddress
  private val callerAcc = secondAddress


  test("calculateAssetId should return right unique id for each invoke") {

    sender.setScript(
      smartAcc,
      Some(ScriptCompiler.compile(dAppV4, ScriptEstimatorV3).explicitGet()._1.bytes().base64),
      fee = setScriptFee + smartFee,
      waitForTx = true
    )
    sender
      .invokeScript(
        callerAcc,
        smartAcc,
        Some("i"),
        args = List.empty,
        fee = smartMinFee + minFee, // dAppV4 contains 1 Issue action
        waitForTx = true
      )
    val assetId = Base58.encode(sender.getDataByKey(smartAcc, "id").as[BinaryDataEntry].value)

    sender
      .invokeScript(
        callerAcc,
        smartAcc,
        Some("i"),
        args = List.empty,
        fee = smartMinFee + minFee, // dAppV4 contains 1 Issue action
        waitForTx = true
      )
    val secondAssetId = Base58.encode(sender.getDataByKey(smartAcc, "id").as[BinaryDataEntry].value)

    sender.assetBalance(smartAcc, assetId).balance shouldBe 100
    sender.assetBalance(smartAcc, secondAssetId).balance shouldBe 100

    val assetDetails = sender.assetsDetails(assetId)
    assetDetails.decimals shouldBe decimals
    assetDetails.name shouldBe assetName
    assetDetails.reissuable shouldBe reissuable
    assetDetails.description shouldBe assetDescr
    assetDetails.minSponsoredAssetFee shouldBe None

  }
}

object InvokeCalcIssueSuite {

  val assetName = "InvokeAsset"
  val assetDescr = "Invoke asset descr"
  val amount = 100
  val decimals = 0
  val reissuable = true

  private val dAppV4: String =
    s"""{-# STDLIB_VERSION 4 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |
      |@Callable(i)
      |func i() = {
      |let issue = Issue("$assetName", "$assetDescr", $amount, $decimals, $reissuable, unit, 0)
      |let id = calculateAssetId(issue)
      |[issue,
      | BinaryEntry("id", id)]
      |}
      |
      |""".stripMargin
}
