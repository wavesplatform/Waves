package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.common.utils.EitherExt2.*
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.{issueFee, scriptBase64, setAssetScriptFee, someAssetAmount}
import com.wavesplatform.it.{BaseFunSuite, NodeConfigs}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state.Height
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class SetAssetScriptTxFeatureSuite extends BaseFunSuite {

  private val featureActivationHeight = Height(11)

  private var assetId = ""

  import NodeConfigs.*
  override protected def nodeConfigs: Seq[Config] =
    Seq(BiggestMiner.quorum(0).preactivatedFeatures((BlockchainFeatures.SmartAssets, featureActivationHeight)))

  override def beforeAll(): Unit = {
    super.beforeAll()

    assetId = sender
      .issue(
        miner.keyPair,
        "SetAssetScript",
        "Test coin for SetAssetScript tests",
        someAssetAmount,
        0,
        reissuable = false,
        issueFee,
        2,
        Some(scriptBase64)
      )
      .id

    nodes.waitForHeightAriseAndTxPresent(assetId)
  }

  test("cannot transact without activated feature") {
    assertBadRequestAndResponse(
      sender.setAssetScript(assetId, miner.keyPair, setAssetScriptFee, Some(scriptBase64)).id,
      s"${BlockchainFeatures.SmartAssets.description} feature has not been activated yet"
    )
  }

  test("can transact after feature activation") {
    nodes.waitForHeight(featureActivationHeight)

    sender.featureActivationStatus(BlockchainFeatures.SmartAssets.id).blockchainStatus shouldBe BlockchainFeatureStatus.Activated

    val script = ScriptCompiler
      .compile(
        s"""
           |match tx {
           |  case _: SetAssetScriptTransaction => true
           |  case _ => false
           |}""".stripMargin,
        ScriptEstimatorV2
      )
      .explicitGet()
      ._1
      .bytes()
      .base64

    val txId = sender
      .setAssetScript(
        assetId,
        miner.keyPair,
        setAssetScriptFee,
        Some(script)
      )
      .id

    nodes.waitForHeightAriseAndTxPresent(txId)
  }
}
