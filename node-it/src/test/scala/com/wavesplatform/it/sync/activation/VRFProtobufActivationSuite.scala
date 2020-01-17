package com.wavesplatform.it.sync.activation

import com.typesafe.config.Config
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite

import scala.concurrent.duration._



class VRFProtobufActivationSuite extends BaseTransactionSuite {
  val activationHeight = 9
  val updateInterval = 2
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.BlockV5.id, activationHeight)))
      .overrideBase(_.raw(s"waves.blockchain.custom.functionality.min-asset-info-update-interval = $updateInterval"))
      .buildNonConflicting()

  private val issuer  = pkByAddress(firstAddress)
  var assetId = ""

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    val (defaultName, defaultDescription) = ("asset", "description")
    assetId = sender.broadcastIssue(issuer, defaultName, defaultDescription, someAssetAmount, 8, true, script = None, waitForTx = true).id
  }

  test("not able to broadcast UpdateAssetInfoTransaction before activation") {
    sender.waitForHeight(sender.transactionInfo(assetId).height + updateInterval + 1)
    assertApiError(sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed. Reason: ActivationError(VRF and Protobuf feature has not been activated yet)"
      error.id shouldBe 112
    }
  }

  test("able to broadcast UpdateAssetInfoTransaction after activation") {
    sender.waitForHeight(activationHeight, 2.minutes)
    sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee, waitForTx = true)
  }
}
