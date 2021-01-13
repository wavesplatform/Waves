package com.wavesplatform.it.sync.transactions

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.api.http.ApiError.{InvalidName, StateCheckFailed, TooBigArrayAllocation}
import com.wavesplatform.api.http.requests.UpdateAssetInfoRequest
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs.{Miners, NotMiner}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{Transaction, TransactionInfo}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.duration._
import scala.util.Random

class UpdateAssetInfoTransactionSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  import UpdateAssetInfoTransactionSuite._

  val updateInterval = 5

  override protected def nodeConfigs: Seq[Config] =
    Seq(
      configWithUpdateIntervalSetting(updateInterval).withFallback(Miners.head),
      configWithUpdateIntervalSetting(updateInterval).withFallback(NotMiner)
    )

  private def issuer = firstKeyPair

  private def nonIssuer = secondKeyPair
  private def dApp      = thirdKeyPair
  var assetId           = ""
  var otherAssetId      = ""
  var smartAssetId      = ""
  var nftId             = ""

  val testDapp =
    """
    {-# STDLIB_VERSION 4 #-}
    {-# CONTENT_TYPE DAPP #-}
    {-# SCRIPT_TYPE ACCOUNT #-}
 
    @Callable(i)
    func isAssetInfoCorrect(id: String, name: String, description: String, quantity: Int, decimals: Int, issuer: String, issuerPublicKey: ByteVector, reissuable: Boolean, scripted: Boolean) = {
      let isCorrect = match assetInfo(fromBase58String(id)) {
        case a:Asset => a.name == name &&
          a.description == description &&
          a.quantity == quantity &&
          a.id == fromBase58String(id) &&
          a.decimals == decimals &&
          a.issuer == Address(fromBase58String(issuer)) &&
          a.issuerPublicKey == issuerPublicKey &&
          a.reissuable == reissuable &&
          a.scripted == scripted &&
          a.minSponsoredFee == unit
      case _ => throw("Can't find asset with specified id")
      }
      [BooleanEntry("isAssetInfoCorrect", isCorrect)]
    }
  """

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    assetId =
      miner.broadcastIssue(issuer, "asset", "description", someAssetAmount, decimals = 8, reissuable = true, script = None, waitForTx = true).id
    otherAssetId = miner
      .broadcastIssue(issuer, "otherAsset", "otherDescription", someAssetAmount, decimals = 8, reissuable = true, script = None, waitForTx = true)
      .id
    smartAssetId = miner
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
    nftId = miner.broadcastIssue(issuer, "asset", "description", quantity = 1, decimals = 0, reissuable = false, script = None, waitForTx = true).id
    val script = ScriptCompiler.compile(testDapp, ScriptEstimatorV3).explicitGet()._1.bytes().base64
    miner.setScript(dApp, Some(script), waitForTx = true)
  }

  var firstUpdateTxId = ""

  test("DApp can read asset info") {
    miner.invokeScript(
      issuer,
      dApp.toAddress.toString,
      func = Some("isAssetInfoCorrect"),
      args = List(
        Terms.CONST_STRING(assetId).explicitGet(),
        Terms.CONST_STRING("asset").explicitGet(),
        Terms.CONST_STRING("description").explicitGet(),
        Terms.CONST_LONG(someAssetAmount),
        Terms.CONST_LONG(8),
        Terms.CONST_STRING(issuer.toAddress.toString).explicitGet(),
        Terms.CONST_BYTESTR(ByteStr(issuer.publicKey.arr)).explicitGet(),
        Terms.CONST_BOOLEAN(true),
        Terms.CONST_BOOLEAN(false)
      ),
      waitForTx = true
    )
    val res = miner.getDataByKey(dApp.toAddress.toString, "isAssetInfoCorrect")
    res.value shouldBe true
  }

  test("able to calculate fee for an update asset tx") {
    val txJson = Json
      .toJson(
        UpdateAssetInfoRequest(
          TxVersion.V1,
          AddressScheme.current.chainId,
          None,
          Some(issuer.publicKey.toString),
          assetId,
          "test",
          "test",
          None,
          0L,
          None,
          None
        )
      )
      .as[JsObject] ++ Json.obj("type" -> UpdateAssetInfoTransaction.typeId)

    val fee = miner.calculateFee(txJson)
    fee.feeAmount shouldBe 1e5.toLong
    fee.feeAssetId shouldBe None
  }

  test("able to update name/description of issued asset") {
    val nextTerm = miner.transactionInfo[TransactionInfo](assetId).height + updateInterval + 1
    nodes.waitForHeight(nextTerm)
    val issuerBalance       = miner.balanceDetails(issuer.publicKey.toAddress.toString)
    firstUpdateTxId = miner.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee)._1.id
    checkUpdateAssetInfoTx(miner.utx().head, "updatedName", "updatedDescription")

    val updateAssetInfoTxInfo = miner.waitForTransaction(firstUpdateTxId)
    val updateAssetInfoHeight = updateAssetInfoTxInfo.height
    checkUpdateAssetInfoTx(miner.blockAt(updateAssetInfoHeight).transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(miner.lastBlock().transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(
      miner.blockSeq(updateAssetInfoHeight, updateAssetInfoHeight).head.transactions.head,
      "updatedName",
      "updatedDescription"
    )
    checkUpdateAssetInfoTx(miner.blockById(miner.lastBlock().id).transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(
      miner.blockSeqByAddress(miner.address, updateAssetInfoHeight, updateAssetInfoHeight).head.transactions.head,
      "updatedName",
      "updatedDescription"
    )

    checkUpdateAssetInfoTxInfo(miner.transactionsByAddress(issuer.publicKey.toAddress.toString, 1).head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTxInfo(updateAssetInfoTxInfo, "updatedName", "updatedDescription")

    miner.assetsDetails(assetId).name shouldBe "updatedName"
    miner.assetsDetails(assetId).description shouldBe "updatedDescription"

    miner.balanceDetails(issuer.publicKey.toAddress.toString).available shouldBe issuerBalance.available - minFee
    nodes.waitForHeightArise()
  }

  test("DApp can read updated asset info") {
    miner.invokeScript(
      issuer,
      dApp.toAddress.toString,
      func = Some("isAssetInfoCorrect"),
      args = List(
        Terms.CONST_STRING(assetId).explicitGet(),
        Terms.CONST_STRING("updatedName").explicitGet(),
        Terms.CONST_STRING("updatedDescription").explicitGet(),
        Terms.CONST_LONG(someAssetAmount),
        Terms.CONST_LONG(8),
        Terms.CONST_STRING(issuer.toAddress.toString).explicitGet(),
        Terms.CONST_BYTESTR(ByteStr(issuer.publicKey.arr)).explicitGet(),
        Terms.CONST_BOOLEAN(true),
        Terms.CONST_BOOLEAN(false)
      ),
      waitForTx = true
    )
    val res = miner.getDataByKey(dApp.toAddress.toString, "isAssetInfoCorrect")
    res.value shouldBe true
  }

  test("not able to update name/description more than once within interval") {
    val nextTermEnd = miner.waitForTransaction(firstUpdateTxId).height + updateInterval
    assertApiError(miner.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include(
        s"Can't update info of asset with id=$assetId before $nextTermEnd block, current height=${miner.height}, minUpdateInfoInterval=$updateInterval"
      )
    }
    miner.waitForHeight(nextTermEnd)
  }

  var secondUpdateInfoHeight = 0

  test("able to update info of other asset after updating info of first asset") {
    val updateAssetInfoTxId = miner.updateAssetInfo(issuer, otherAssetId, "secondUpdate", "secondUpdatedDescription", minFee)._1.id
    miner.waitForTransaction(updateAssetInfoTxId)
    secondUpdateInfoHeight = miner.transactionInfo[TransactionInfo](updateAssetInfoTxId).height
    checkUpdateAssetInfoTx(miner.blockAt(secondUpdateInfoHeight).transactions.head, "secondUpdate", "secondUpdatedDescription")
    checkUpdateAssetInfoTx(
      miner.blockSeq(secondUpdateInfoHeight, secondUpdateInfoHeight).head.transactions.head,
      "secondUpdate",
      "secondUpdatedDescription"
    )
    checkUpdateAssetInfoTx(
      miner.blockSeqByAddress(miner.address, secondUpdateInfoHeight, secondUpdateInfoHeight).head.transactions.head,
      "secondUpdate",
      "secondUpdatedDescription"
    )

    checkUpdateAssetInfoTxInfo(miner.transactionInfo[TransactionInfo](updateAssetInfoTxId), "secondUpdate", "secondUpdatedDescription")

    miner.assetsDetails(otherAssetId).name shouldBe "secondUpdate"
    miner.assetsDetails(otherAssetId).description shouldBe "secondUpdatedDescription"
  }

  test("able to update asset info after rollback to update height") {
    nodes.blacklistPeersAndRollback(secondUpdateInfoHeight - 1, returnToUTX = false)
    miner.assetsDetails(otherAssetId).name shouldBe "otherAsset"
    miner.assetsDetails(otherAssetId).description shouldBe "otherDescription"

    miner.updateAssetInfo(issuer, otherAssetId, "secondUpdate", "secondUpdatedDescription", minFee, waitForTx = true)
  }

  test("able to update asset info after rollback to issue height") {
    val assetId                                       = miner.broadcastIssue(issuer, "asset", "description", 1, 0, false, script = None, waitForTx = true).id
    val issueHeight                                   = miner.transactionInfo[TransactionInfo](assetId).height
    val (firstUpdatedName, firstUpdatedDescription)   = ("updatedName", "updatedDescription")
    val (secondUpdatedName, secondUpdatedDescription) = ("updatedName2", "updatedDescription2")

    miner.waitForHeight(miner.height + updateInterval + 1, 3.minutes)
    miner.updateAssetInfo(issuer, assetId, firstUpdatedName, firstUpdatedDescription, waitForTx = true)._1.id
    nodes.blacklistPeersAndRollback(issueHeight - 1)

    miner.waitForTransaction(assetId)
    val newIssueHeight = miner.transactionInfo[TransactionInfo](assetId).height
    miner.waitForHeight(newIssueHeight + updateInterval + 1, 3.minutes)
    miner.updateAssetInfo(issuer, assetId, secondUpdatedName, secondUpdatedDescription, waitForTx = true)

    miner.assetsDetails(assetId).name shouldBe secondUpdatedName
    miner.assetsDetails(assetId).description shouldBe secondUpdatedDescription
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
      miner.waitForHeight(miner.height + updateInterval + 1, 3.minutes)
      assertApiError(
        miner.updateAssetInfo(issuer, assetId, assetName, "updatedDescription", minFee),
        InvalidName
      )
    }
  }

  test("not able to set too big description") {
    val tooBigDescription = Random.nextString(1001)
    assertApiError(
      miner.updateAssetInfo(issuer, assetId, "updatedName", tooBigDescription, minFee),
      TooBigArrayAllocation
    )
  }

  test("not able to update asset info without paying enough fee") {
    assertApiError(miner.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee - 1)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message shouldBe s"State check failed. Reason: . Fee for UpdateAssetInfoTransaction (${minFee - 1} in WAVES) does not exceed minimal value of $minFee WAVES."
    }
  }

  test("not able to update info of not-issued asset") {
    val notIssuedAssetId = "BzARFPgBqWFu6MHGxwkPVKmaYAzyShu495Ehsgru72Wz"
    assertApiError(miner.updateAssetInfo(issuer, notIssuedAssetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message shouldBe "State check failed. Reason: Referenced assetId not found"
    }
  }

  test("non-issuer cannot update asset info") {
    assertApiError(miner.updateAssetInfo(nonIssuer, assetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include("Asset was issued by other address")
    }
  }

  test("asset script can read asset info") {
    val scriptTextT = s"""true""".stripMargin
    val scriptT     = ScriptCompiler(scriptTextT, isAssetScript = true, ScriptEstimatorV2).explicitGet()._1.bytes().base64

    val smartAssetId1 =
      miner.broadcastIssue(issuer, "smartAsset", "description", someAssetAmount, 8, reissuable = true, script = Some(scriptT), waitForTx = true).id
    val scriptText1 = s"""
          |{-# STDLIB_VERSION 4 #-}
          |{-# CONTENT_TYPE EXPRESSION #-}
          |{-# SCRIPT_TYPE ASSET #-}
 
          |match assetInfo(fromBase58String("${smartAssetId1}")) {
          |case a:Asset =>
          | a.name == "smartAsset" &&
          | this.name == "smartAsset" &&
          | a.description == "description" &&
          | this.description == "description" &&
          | a.quantity == ${someAssetAmount} &&
          | a.quantity == ${someAssetAmount} &&
          | this.id == fromBase58String("${smartAssetId1}") &&
          | a.decimals == 8 &&
          | this.decimals == 8 &&
          | a.issuer == Address(fromBase58String("${issuer.toAddress.toString}")) &&
          | this.issuer == Address(fromBase58String("${issuer.toAddress.toString}")) &&
          | a.issuerPublicKey == this.issuerPublicKey &&
          | a.reissuable == true &&
          | this.reissuable == true &&
          | a.scripted == true &&
          | this.scripted == true &&
          | a.minSponsoredFee == unit &&
          | this.minSponsoredFee == unit
          |case _ => false
          |}""".stripMargin
    val script1     = ScriptCompiler(scriptText1, isAssetScript = true, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    miner.setAssetScript(smartAssetId1, issuer, setAssetScriptFee, Some(script1), waitForTx = true)

    miner.burn(issuer, smartAssetId1, 1, minFee + 2 * smartFee, waitForTx = true)
  }

  test("check increased fee for smart sender/asset") {
    val scriptText = s"""true""".stripMargin
    val script     = ScriptCompiler(scriptText, isAssetScript = true, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    val smartAssetId =
      miner.broadcastIssue(issuer, "smartAsset", "description", someAssetAmount, 8, reissuable = true, script = Some(script), waitForTx = true).id
    miner.waitForHeight(miner.height + updateInterval + 1, 3.minutes)
    assertApiError(miner.updateAssetInfo(issuer, smartAssetId, "updatedName", "updatedDescription", minFee + smartFee - 1)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message shouldBe s"State check failed. Reason: Transaction involves 1 scripted assets. Requires $smartFee extra fee." +
        s" Fee for UpdateAssetInfoTransaction (${smartMinFee - 1} in WAVES) does not exceed minimal value of $smartMinFee WAVES."
    }
    miner.setScript(issuer, Some(script), waitForTx = true)
    assertApiError(miner.updateAssetInfo(issuer, smartAssetId, "updatedName", "updatedDescription", minFee + 2 * smartFee - 1)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message shouldBe s"State check failed. Reason: Transaction sent from smart account. Requires $smartFee extra fee." +
        s" Transaction involves 1 scripted assets. Requires $smartFee extra fee." +
        s" Fee for UpdateAssetInfoTransaction (${smartMinFee + smartFee - 1} in WAVES) does not exceed minimal value of ${smartMinFee + smartFee} WAVES."
    }

    miner.updateAssetInfo(issuer, smartAssetId, "updatedName", "updatedDescription", minFee + 2 * smartFee, waitForTx = true)
    nodes.waitForHeightArise()
  }

  test("able to update name/description of nft") {
    val updateAssetInfoTxId = miner.updateAssetInfo(issuer, nftId, "updatedName", "updatedDescription", minFee + smartFee)._1.id
    checkUpdateAssetInfoTx(miner.utx().head, "updatedName", "updatedDescription")
    miner.waitForTransaction(updateAssetInfoTxId)
    val updateAssetInfoTxHeight = miner.transactionInfo[TransactionInfo](updateAssetInfoTxId).height
    checkUpdateAssetInfoTx(miner.blockAt(updateAssetInfoTxHeight).transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(miner.lastBlock().transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(
      miner.blockSeq(updateAssetInfoTxHeight, updateAssetInfoTxHeight).head.transactions.head,
      "updatedName",
      "updatedDescription"
    )
    checkUpdateAssetInfoTx(miner.blockById(miner.lastBlock().id).transactions.head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTx(
      miner.blockSeqByAddress(miner.address, updateAssetInfoTxHeight, updateAssetInfoTxHeight).head.transactions.head,
      "updatedName",
      "updatedDescription"
    )

    checkUpdateAssetInfoTxInfo(miner.transactionsByAddress(issuer.publicKey.toAddress.toString, 1).head, "updatedName", "updatedDescription")
    checkUpdateAssetInfoTxInfo(miner.transactionInfo[TransactionInfo](updateAssetInfoTxId), "updatedName", "updatedDescription")

    miner.assetsDetails(nftId).name shouldBe "updatedName"
    miner.assetsDetails(nftId).description shouldBe "updatedDescription"
  }

  test("reissue/burn/setassetscript should not affect update interval") {
    miner.reissue(issuer, assetId, 100, reissuable = true, version = TxVersion.V2, waitForTx = true)
    miner.updateAssetInfo(issuer, assetId, "afterReissue", "asset after reissue", waitForTx = true)
    miner.assetsDetails(assetId).name shouldBe "afterReissue"
    miner.assetsDetails(assetId).description shouldBe "asset after reissue"

    miner.waitForHeight(miner.height + updateInterval + 1, 2.minutes)
    miner.burn(issuer, assetId, 100, version = TxVersion.V2, fee = smartMinFee, waitForTx = true)
    miner.updateAssetInfo(issuer, assetId, "afterBurn", "asset after burn", waitForTx = true)
    miner.assetsDetails(assetId).name shouldBe "afterBurn"
    miner.assetsDetails(assetId).description shouldBe "asset after burn"

    miner.waitForHeight(miner.height + updateInterval + 1, 2.minutes)
    miner.setAssetScript(
      smartAssetId,
      issuer,
      script = Some(scriptBase64),
      version = TxVersion.V2,
      fee = setAssetScriptFee + 2 * smartFee,
      waitForTx = true
    )
    miner.updateAssetInfo(issuer, smartAssetId, "afterSAScript", "asset after set asset script", waitForTx = true)
    miner.assetsDetails(smartAssetId).name shouldBe "afterSAScript"
    miner.assetsDetails(smartAssetId).description shouldBe "asset after set asset script"

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
      s"""waves {
         |   blockchain.custom {
         |      functionality {
         |        min-asset-info-update-interval = $interval
         |      }
         |   }
         |   miner.quorum = 0
         |}""".stripMargin
    )
}
