package com.wavesplatform.it.sync.transactions

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.api.http.ApiError.{InvalidName, StateCheckFailed, TooBigArrayAllocation}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs.{Miners, NotMiner}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{Transaction, TransactionInfo}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration._
import scala.util.Random

class UpdateAssetInfoTransactionSuite extends BaseTransactionSuite with CancelAfterFailure with TableDrivenPropertyChecks {
  import UpdateAssetInfoTransactionSuite._
  val updateInterval = 2
  override protected def nodeConfigs: Seq[Config] =
    Seq(
      configWithUpdateIntervalSetting(updateInterval).withFallback(Miners.head),
      configWithUpdateIntervalSetting(updateInterval).withFallback(NotMiner)
    )

  val issuer       = pkByAddress(firstAddress)
  val nonIssuer    = pkByAddress(secondAddress)
  var assetId      = ""
  var otherAssetId = ""
  var smartAssetId = ""
  var nftId        = ""

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    assetId =
      sender.broadcastIssue(issuer, "asset", "description", someAssetAmount, decimals = 8, reissuable = true, script = None, waitForTx = true).id
    otherAssetId = sender
      .broadcastIssue(issuer, "otherAsset", "otherDescription", someAssetAmount, decimals = 8, reissuable = true, script = None, waitForTx = true)
      .id
    smartAssetId = sender
      .broadcastIssue(
        issuer,
        "smartAsset",
        "smartDescription",
        someAssetAmount,
        decimals = 8,
        reissuable = true,
        script = Some(scriptBase64),
        waitForTx = true
      )
      .id
    nftId = sender.broadcastIssue(issuer, "asset", "description", quantity = 1, decimals = 0, reissuable = false, script = None, waitForTx = true).id
  }

  test("able to update name/description of issued asset") {
    val nextTerm = sender.transactionInfo[TransactionInfo](assetId).height + updateInterval + 1
    nodes.waitForHeight(nextTerm)
    val issuerBalance       = sender.balanceDetails(issuer.publicKey.toAddress.toString)
    val updateAssetInfoTxId = notMiner.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee)._1.id
    checkUpdateAssetInfoTx(notMiner.utx().head, "updatedName", "updatedDescription")
    miner.waitForTransaction(updateAssetInfoTxId)
    val updateAssetInfoTxHeight = sender.transactionInfo[TransactionInfo](updateAssetInfoTxId).height
    checkUpdateAssetInfoTx(sender.blockAt(updateAssetInfoTxHeight).transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(sender.lastBlock().transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(
      sender.blockSeq(updateAssetInfoTxHeight, updateAssetInfoTxHeight).head.transactions.head,
      "updatedName",
      "updatedDescription"
    )
    checkUpdateAssetInfoTx(sender.blockById(sender.lastBlock().id).transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(
      sender.blockSeqByAddress(miner.address, updateAssetInfoTxHeight, updateAssetInfoTxHeight).head.transactions.head,
      "updatedName",
      "updatedDescription"
    )

    checkUpdateAssetInfoTxInfo(sender.transactionsByAddress(issuer.publicKey.toAddress.toString, 1).head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTxInfo(sender.transactionInfo[TransactionInfo](updateAssetInfoTxId), "updatedName", "updatedDescription")

    sender.assetsDetails(assetId).name shouldBe "updatedName"
    sender.assetsDetails(assetId).description shouldBe "updatedDescription"

    sender.balanceDetails(issuer.publicKey.toAddress.toString).available shouldBe issuerBalance.available - minFee
    nodes.waitForHeightArise()
  }

  test("not able to update name/description more than once within interval") {
    val nextTermEnd = sender.transactionInfo[TransactionInfo](assetId).height + 2 * updateInterval
    assertApiError(sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include(
        s"Can't update info of asset with id=$assetId before ${nextTermEnd + 1} block, current height=${sender.height}, minUpdateInfoInterval=$updateInterval"
      )
    }
    sender.waitForHeight(nextTermEnd)

    assertApiError(sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include(
        s"Can't update info of asset with id=$assetId before ${nextTermEnd + 1} block, current height=${sender.height}, minUpdateInfoInterval=$updateInterval"
      )
    }
  }

  var secondUpdateInfoHeight = 0

  test("able to update info of other asset after updating info of first asset") {
    val updateAssetInfoTxId = sender.updateAssetInfo(issuer, otherAssetId, "secondUpdate", "secondUpdatedDescription", minFee)._1.id
    sender.waitForUtxIncreased(0)
    checkUpdateAssetInfoTx(sender.utx().head, "secondUpdate", "secondUpdatedDescription")
    sender.waitForTransaction(updateAssetInfoTxId)
    secondUpdateInfoHeight = sender.transactionInfo[TransactionInfo](updateAssetInfoTxId).height
    checkUpdateAssetInfoTx(sender.blockAt(secondUpdateInfoHeight).transactions.head, "secondUpdate", "secondUpdatedDescription")
    checkUpdateAssetInfoTx(sender.lastBlock().transactions.head, "secondUpdate", "secondUpdatedDescription")
    checkUpdateAssetInfoTx(
      sender.blockSeq(secondUpdateInfoHeight, secondUpdateInfoHeight).head.transactions.head,
      "secondUpdate",
      "secondUpdatedDescription"
    )
    checkUpdateAssetInfoTx(sender.blockById(sender.lastBlock().id).transactions.head, "secondUpdate", "secondUpdatedDescription")
    checkUpdateAssetInfoTx(
      sender.blockSeqByAddress(miner.address, secondUpdateInfoHeight, secondUpdateInfoHeight).head.transactions.head,
      "secondUpdate",
      "secondUpdatedDescription"
    )

    checkUpdateAssetInfoTxInfo(sender.transactionsByAddress(issuer.publicKey.toAddress.toString, 1).head, "secondUpdate", "secondUpdatedDescription")
    checkUpdateAssetInfoTxInfo(sender.transactionInfo[TransactionInfo](updateAssetInfoTxId), "secondUpdate", "secondUpdatedDescription")

    sender.assetsDetails(otherAssetId).name shouldBe "secondUpdate"
    sender.assetsDetails(otherAssetId).description shouldBe "secondUpdatedDescription"
  }

  test("able to update asset info after rollback to update height") {
    nodes.rollback(secondUpdateInfoHeight - 1, returnToUTX = false)
    sender.assetsDetails(otherAssetId).name shouldBe "otherAsset"
    sender.assetsDetails(otherAssetId).description shouldBe "otherDescription"

    sender.updateAssetInfo(issuer, otherAssetId, "secondUpdate", "secondUpdatedDescription", minFee, waitForTx = true)
  }

  test("able to update asset info after rollback to issue height") {
    val assetId                                       = sender.broadcastIssue(issuer, "asset", "description", 1, 0, false, script = None, waitForTx = true).id
    val issueHeight                                   = sender.transactionInfo[TransactionInfo](assetId).height
    val (firstUpdatedName, firstUpdatedDescription)   = ("updatedName", "updatedDescription")
    val (secondUpdatedName, secondUpdatedDescription) = ("updatedName2", "updatedDescription2")

    sender.waitForHeight(sender.height + updateInterval + 1, 3.minutes)
    sender.updateAssetInfo(issuer, assetId, firstUpdatedName, firstUpdatedDescription, waitForTx = true)._1.id
    nodes.rollback(issueHeight - 1, returnToUTX = true)

    sender.waitForTransaction(assetId)
    val newIssueHeight = sender.transactionInfo[TransactionInfo](assetId).height
    sender.waitForHeight(newIssueHeight + updateInterval + 1, 3.minutes)
    sender.updateAssetInfo(issuer, assetId, secondUpdatedName, secondUpdatedDescription, waitForTx = true)

    sender.assetsDetails(assetId).name shouldBe secondUpdatedName
    sender.assetsDetails(assetId).description shouldBe secondUpdatedDescription
  }

  val invalidAssetsNames =
    Table(
      "",
      "abc",
      "NameIsLongerThanLimit",
      "~!|#$%^&*()_+=\";:/?><|\\][{}"
    )

  forAll(invalidAssetsNames) { assetName: String =>
    test(s"not able to update name to $assetName") {
      sender.waitForHeight(sender.height + updateInterval + 1, 3.minutes)
      assertApiError(
        sender.updateAssetInfo(issuer, assetId, assetName, "updatedDescription", minFee),
        InvalidName
      )
    }
  }

  test("not able to set too big description") {
    val tooBigDescription = Random.nextString(1001)
    assertApiError(
      sender.updateAssetInfo(issuer, assetId, "updatedName", tooBigDescription, minFee),
      TooBigArrayAllocation
    )
  }

  test("not able to update asset info without paying enough fee") {
    assertApiError(sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee - 1)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message shouldBe s"State check failed. Reason: . Fee for UpdateAssetInfoTransaction (${minFee - 1} in WAVES) does not exceed minimal value of $minFee WAVES."
    }
  }

  test("not able to update info of not-issued asset") {
    val notIssuedAssetId = "BzARFPgBqWFu6MHGxwkPVKmaYAzyShu495Ehsgru72Wz"
    assertApiError(sender.updateAssetInfo(issuer, notIssuedAssetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message shouldBe "State check failed. Reason: Referenced assetId not found"
    }
  }

  test("non-issuer cannot update asset info") {
    assertApiError(sender.updateAssetInfo(nonIssuer, assetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include("Asset was issued by other address")
    }
  }

  test("check increased fee for smart sender/asset") {
    val scriptText = s"""true""".stripMargin
    val script     = ScriptCompiler(scriptText, isAssetScript = true, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    val smartAssetId =
      sender.broadcastIssue(issuer, "smartAsset", "description", someAssetAmount, 8, reissuable = true, script = Some(script), waitForTx = true).id
    sender.waitForHeight(sender.height + updateInterval + 1, 3.minutes)
    assertApiError(sender.updateAssetInfo(issuer, smartAssetId, "updatedName", "updatedDescription", minFee + smartFee - 1)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message shouldBe s"State check failed. Reason: Transaction involves 1 scripted assets. Requires $smartFee extra fee." +
        s" Fee for UpdateAssetInfoTransaction (${smartMinFee - 1} in WAVES) does not exceed minimal value of $smartMinFee WAVES."
    }
    sender.setScript(issuer.publicKey.toAddress.toString, Some(script), waitForTx = true)
    assertApiError(sender.updateAssetInfo(issuer, smartAssetId, "updatedName", "updatedDescription", minFee + 2 * smartFee - 1)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message shouldBe s"State check failed. Reason: Transaction sent from smart account. Requires $smartFee extra fee." +
        s" Transaction involves 1 scripted assets. Requires $smartFee extra fee." +
        s" Fee for UpdateAssetInfoTransaction (${smartMinFee + smartFee - 1} in WAVES) does not exceed minimal value of ${smartMinFee + smartFee} WAVES."
    }

    sender.updateAssetInfo(issuer, smartAssetId, "updatedName", "updatedDescription", minFee + 2 * smartFee, waitForTx = true)
    nodes.waitForHeightArise()
  }

  test("able to update name/description of nft") {
    val updateAssetInfoTxId = sender.updateAssetInfo(issuer, nftId, "updatedName", "updatedDescription", minFee + smartFee)._1.id
    checkUpdateAssetInfoTx(sender.utx().head, "updatedName", "updatedDescription")
    sender.waitForTransaction(updateAssetInfoTxId)
    val updateAssetInfoTxHeight = sender.transactionInfo[TransactionInfo](updateAssetInfoTxId).height
    checkUpdateAssetInfoTx(sender.blockAt(updateAssetInfoTxHeight).transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(sender.lastBlock().transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(
      sender.blockSeq(updateAssetInfoTxHeight, updateAssetInfoTxHeight).head.transactions.head,
      "updatedName",
      "updatedDescription"
    )
    checkUpdateAssetInfoTx(sender.blockById(sender.lastBlock().id).transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(
      sender.blockSeqByAddress(miner.address, updateAssetInfoTxHeight, updateAssetInfoTxHeight).head.transactions.head,
      "updatedName",
      "updatedDescription"
    )

    checkUpdateAssetInfoTxInfo(sender.transactionsByAddress(issuer.publicKey.toAddress.toString, 1).head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTxInfo(sender.transactionInfo[TransactionInfo](updateAssetInfoTxId), "updatedName", "updatedDescription")

    sender.assetsDetails(nftId).name shouldBe "updatedName"
    sender.assetsDetails(nftId).description shouldBe "updatedDescription"
  }

  test("reissue/burn/setassetscript should not affect update interval") {
    sender.reissue(issuer.toAddress.toString, assetId, 100, reissuable = true, version = TxVersion.V2, waitForTx = true)
    sender.updateAssetInfo(issuer, assetId, "afterReissue", "asset after reissue", waitForTx = true)
    sender.assetsDetails(assetId).name shouldBe "afterReissue"
    sender.assetsDetails(assetId).description shouldBe "asset after reissue"

    sender.waitForHeight(sender.height + updateInterval + 1, 2.minutes)
    sender.burn(issuer.toAddress.toString, assetId, 100, version = TxVersion.V2, fee = smartMinFee, waitForTx = true)
    sender.updateAssetInfo(issuer, assetId, "afterBurn", "asset after burn", waitForTx = true)
    sender.assetsDetails(assetId).name shouldBe "afterBurn"
    sender.assetsDetails(assetId).description shouldBe "asset after burn"

    sender.waitForHeight(sender.height + updateInterval + 1, 2.minutes)
    sender.setAssetScript(
      smartAssetId,
      issuer.toAddress.toString,
      script = Some(scriptBase64),
      version = TxVersion.V2,
      fee = setAssetScriptFee + 2 * smartFee,
      waitForTx = true
    )
    sender.updateAssetInfo(issuer, smartAssetId, "afterSAScript", "asset after set asset script", waitForTx = true)
    sender.assetsDetails(smartAssetId).name shouldBe "afterSAScript"
    sender.assetsDetails(smartAssetId).description shouldBe "asset after set asset script"

  }

  def checkUpdateAssetInfoTx(transaction: Transaction, updatedName: String, updatedDescription: String): Unit = {
    transaction._type shouldBe 17
    transaction.name.get shouldBe updatedName
    transaction.description.get shouldBe updatedDescription
  }

  def checkUpdateAssetInfoTxInfo(transactionInfo: TransactionInfo, updatedName: String, updatedDescription: String): Unit = {
    transactionInfo._type shouldBe 17
    transactionInfo.name.get shouldBe updatedName
    transactionInfo.description.get shouldBe updatedDescription
  }

}

object UpdateAssetInfoTransactionSuite {

  private def configWithUpdateIntervalSetting(interval: Long) =
    ConfigFactory.parseString(
      s"""
         |waves {
         |   blockchain.custom {
         |      functionality {
         |        min-asset-info-update-interval = $interval
         |      }
         |   }
         |   miner.quorum = 0
         |}""".stripMargin
    )
}
