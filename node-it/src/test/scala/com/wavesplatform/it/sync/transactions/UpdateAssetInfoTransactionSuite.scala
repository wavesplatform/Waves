package com.wavesplatform.it.sync.transactions

import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.config.ConfigFactory.parseString
import com.wavesplatform.account.KeyPair
import com.wavesplatform.api.http.ApiError.{InvalidName, StateCheckFailed, TooBigArrayAllocation}
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.{Miners, NotMiner}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{Transaction, TransactionInfo}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import org.scalatest.CancelAfterFailure
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration._
import scala.util.Random

class UpdateAssetInfoTransactionSuite extends BaseTransactionSuite with CancelAfterFailure with TableDrivenPropertyChecks {
  import UpdateAssetInfoTransactionSuite._
  val updateInterval = 2
  override protected def nodeConfigs: Seq[Config] = Seq(configWithUpdateIntervalSetting(updateInterval).withFallback(Miners.head))

  val issuer    = pkByAddress(firstAddress)
  val nonIssuer = pkByAddress(secondAddress)
  var assetId   = ""

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    assetId = sender.broadcastIssue(issuer, "asset", "description", someAssetAmount, 8, true, script = None, waitForTx = true).id
  }

  test("able to update name/description of issued asset") {
    val nextTerm = sender.transactionInfo(assetId).height + updateInterval + 1
    sender.waitForHeight(nextTerm, 3.minutes)
    val issuerBalance = sender.balanceDetails(issuer.publicKey.stringRepr)
    val updateAssetInfoTxId = sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee).id
    checkUpdateAssetInfoTx(sender.utx.head, "updatedName", "updatedDescription")
    sender.waitForTransaction(updateAssetInfoTxId)
    val updateAssetInfoTxHeight = sender.transactionInfo(updateAssetInfoTxId).height
    checkUpdateAssetInfoTx(sender.blockAt(updateAssetInfoTxHeight).transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(sender.lastBlock.transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(sender.blockSeq(updateAssetInfoTxHeight, updateAssetInfoTxHeight).head.transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(sender.blockBySignature(sender.lastBlock.signature).transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(sender.blockSeqByAddress(miner.address, updateAssetInfoTxHeight, updateAssetInfoTxHeight).head.transactions.head, "updatedName", "updatedDescription")

    checkUpdateAssetInfoTxInfo(sender.transactionsByAddress(issuer.publicKey.stringRepr, 1).head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTxInfo(sender.transactionInfo(updateAssetInfoTxId), "updatedName", "updatedDescription")

    sender.assetsDetails(assetId).name shouldBe "updatedName"
    sender.assetsDetails(assetId).description shouldBe "updatedDescription"

    sender.balanceDetails(issuer.publicKey.stringRepr).available shouldBe issuerBalance.available - minFee
  }

  test("not able to update name/description more than once within interval") {
    val nextTermEnd = sender.transactionInfo(assetId).height + 2 * updateInterval
    assertApiError(sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include("Can't update asset info before")
    }
    sender.waitForHeight(nextTermEnd)

    assertApiError(sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include("Can't update asset info before")
    }
  }

  val invalidAssetsNames =
    Table(
      "",
      null,
      "abc",
      "NameIsLongerThanLimit",
      "\\uDC00",
      "~!|#$%^&*()_+=\";:/?><|\\][{}"
    )

  forAll(invalidAssetsNames) { assetName: String =>
    test(s"not able to update name to $assetName") {
      sender.waitForHeight(sender.height + 3, 3.minutes)
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

  def checkUpdateAssetInfoTx(transaction: Transaction, updatedName: String, updatedDescription: String): Unit ={
    transaction.`type` shouldBe 17
    transaction.name.get shouldBe updatedName
    transaction.description.get shouldBe updatedDescription
  }

  def checkUpdateAssetInfoTxInfo(transactionInfo: TransactionInfo, updatedName: String, updatedDescription: String): Unit ={
    transactionInfo.`type` shouldBe 17
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
