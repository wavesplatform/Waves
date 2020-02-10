package com.wavesplatform.it.sync.activation

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.StateCheckFailed
import com.wavesplatform.features.{BlockchainFeature, BlockchainFeatures}
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.transaction.TxVersion
import scala.concurrent.duration._

class VRFProtobufActivationSuite extends BaseTransactionSuite {
  val activationHeight = 9
  val updateInterval = 3
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.Builder(Default, 1, Seq.empty)
      .overrideBase(_.quorum(0))
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.BlockV5.id, activationHeight)))
      .overrideBase(_.raw(s"waves.blockchain.custom.functionality.min-asset-info-update-interval = $updateInterval"))
      .buildNonConflicting()

  private val senderAcc  = pkByAddress(firstAddress)
  private val recipientAcc = pkByAddress(secondAddress)
  var assetId = ""
  var otherAssetId = ""

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    val (defaultName, defaultDescription) = ("asset", "description")
    assetId = sender.broadcastIssue(senderAcc, defaultName, defaultDescription, someAssetAmount, 8, true, script = None, waitForTx = true).id
    sender.waitForHeight(7, 3.minutes)
    otherAssetId = sender.broadcastIssue(senderAcc, defaultName, defaultDescription, someAssetAmount, 8, true, script = None, waitForTx = true).id
  }

  test("not able to broadcast tx of new versions before activation") {
    assertApiError(sender.transfer(senderAcc.stringRepr, recipientAcc.stringRepr, transferAmount, version = TxVersion.V3)) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed. Reason: ActivationError(VRF and Protobuf feature has not been activated yet)"
      error.id shouldBe 112
    }
  }

  test("not able to broadcast UpdateAssetInfoTransaction before activation") {
    assertApiError(sender.updateAssetInfo(senderAcc, otherAssetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "State check failed. Reason: ActivationError(VRF and Protobuf feature has not been activated yet)"
      error.id shouldBe 112
    }
  }

  test("not able to update asset info after activation if update interval has not been reached after asset issue") {
    sender.waitForHeight(activationHeight, 2.minutes)
    assertApiError(sender.updateAssetInfo(senderAcc, otherAssetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include(s"Can't update info of asset with id=$otherAssetId")
    }
  }

  test("able to broadcast UpdateAssetInfoTransaction if interval's reached before activation") {
    sender.updateAssetInfo(senderAcc, assetId, "updatedName", "updatedDescription", minFee, waitForTx = true)
  }

  test("able to broadcast UpdateAssetInfoTransaction after activation") {
    val nextTerm = sender.transactionInfo(otherAssetId).height + updateInterval + 1
    sender.waitForHeight(nextTerm, 2.minutes)
    sender.updateAssetInfo(senderAcc, otherAssetId, "updatedName", "updatedDescription", minFee, waitForTx = true)
  }

  test("able to broadcast tx of new versions before activation") {
    val senderWavesBalance = sender.balanceDetails(senderAcc.stringRepr)
    val recipientWavesBalance = sender.balanceDetails(senderAcc.stringRepr)
    sender.transfer(senderAcc.stringRepr, recipientAcc.stringRepr, transferAmount, version = TxVersion.V3)

    senderWavesBalance.available shouldBe sender.balanceDetails(senderAcc.stringRepr).available - transferAmount - minFee
    recipientWavesBalance.available shouldBe sender.balanceDetails(recipientAcc.stringRepr).available + transferAmount
  }
}
