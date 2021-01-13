package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.utils.EitherExt2
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
import org.scalatest.{Matchers, OptionValues}

class InvokeCalcIssueSuite extends BaseTransactionSuite with Matchers with OptionValues {

  import InvokeCalcIssueSuite._

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.BlockV5, 0)))
      .buildNonConflicting()

  private def smartAcc = firstKeyPair
  private def callerAcc = secondKeyPair

  test("calculateAssetId should return right unique id for each invoke") {

    miner.setScript(
      smartAcc,
      Some(ScriptCompiler.compile(dAppV4, ScriptEstimatorV3).explicitGet()._1.bytes().base64),
      fee = setScriptFee + smartFee,
      waitForTx = true
    )
    val smartAccAddress = smartAcc.toAddress.toString
    miner
      .invokeScript(
        callerAcc,
        smartAccAddress,
        Some("i"),
        args = List.empty,
        fee = invokeFee + issueFee, // dAppV4 contains 1 Issue action
        waitForTx = true
      )
    val assetId = miner.getDataByKey(smartAccAddress, "id").as[BinaryDataEntry].value.toString

    miner
      .invokeScript(
        callerAcc,
        smartAccAddress,
        Some("i"),
        args = List.empty,
        fee = invokeFee + issueFee, // dAppV4 contains 1 Issue action
        waitForTx = true
      )
    val secondAssetId = miner.getDataByKey(smartAccAddress, "id").as[BinaryDataEntry].value.toString

    miner.assetBalance(smartAccAddress, assetId).balance shouldBe 100
    miner.assetBalance(smartAccAddress, secondAssetId).balance shouldBe 100

    val assetDetails = miner.assetsDetails(assetId)
    assetDetails.decimals shouldBe decimals
    assetDetails.name shouldBe assetName
    assetDetails.reissuable shouldBe reissuable
    assetDetails.description shouldBe assetDescr
    assetDetails.minSponsoredAssetFee shouldBe None

  }
}

object InvokeCalcIssueSuite {

  val assetName  = "InvokeAsset"
  val assetDescr = "Invoke asset descr"
  val amount     = 100
  val decimals   = 0
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
