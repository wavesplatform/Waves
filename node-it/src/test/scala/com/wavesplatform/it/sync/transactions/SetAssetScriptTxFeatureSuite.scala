package com.wavesplatform.it.sync.transactions

import com.typesafe.config.Config
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{issueFee, scriptBase64, setAssetScriptFee, someAssetAmount}
import com.wavesplatform.it.transactions.BaseTransactionSuite

class SetAssetScriptTxFeatureSuite extends BaseTransactionSuite {

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw(s"""waves {
                             |  blockchain.custom.functionality {
                             |    pre-activated-features = {
                             |      ${BlockchainFeatures.SmartAssets.id} = ${Int.MaxValue}
                             |    }
                             |  }
                             |}""".stripMargin))
      .withDefault(1)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  test("cannot transact without activated feature") {
    val assetId = sender
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

    assertBadRequestAndResponse(
      sender.setAssetScript(assetId, firstAddress, setAssetScriptFee, Some(scriptBase64)).id,
      s"${BlockchainFeatures.SmartAssets.description} feature has not been activated yet"
    )
  }
}
