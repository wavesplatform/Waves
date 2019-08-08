package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{issueFee, scriptBase64, setAssetScriptFee, someAssetAmount}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.common.utils._
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class SetAssetScriptTxFeatureSuite extends BaseTransactionSuite {

  private val featureActivationHeight = 8

  private var assetId = ""

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw(s"""waves {
                             |  blockchain.custom.functionality {
                             |    pre-activated-features = {
                             |      ${BlockchainFeatures.SmartAssets.id} = $featureActivationHeight
                             |    }
                             |    
                             |  }
                             |}""".stripMargin))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  override def beforeAll(): Unit = {
    super.beforeAll()

    assetId = sender
      .issue(
        firstAddress,
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
      sender.setAssetScript(assetId, firstAddress, setAssetScriptFee, Some(scriptBase64)).id,
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
           |  case s : SetAssetScriptTransaction => true
           |  case _ => false
           |}""".stripMargin
      )
      .explicitGet()
      ._1
      .bytes
      .value
      .base64

    val txId = sender
      .setAssetScript(
        assetId,
        firstAddress,
        setAssetScriptFee,
        Some(script)
      )
      .id

    nodes.waitForHeightAriseAndTxPresent(txId)
  }
}
