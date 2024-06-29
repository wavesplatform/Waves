package com.wavesplatform.it.sync.transactions

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.account.AddressScheme
import com.wavesplatform.api.http.ApiError.{InvalidName, StateCheckFailed, TooBigArrayAllocation}
import com.wavesplatform.api.http.requests.UpdateAssetInfoRequest
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs.{Miners, NotMiner}
import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.api.{Transaction, TransactionInfo}
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.v1.compiler.{Terms, TestCompiler}
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.assets.IssueTransaction.{MaxAssetDescriptionLength, MaxAssetNameLength, MinAssetNameLength}
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import com.wavesplatform.transaction.{TransactionType, TxVersion}
import org.scalatest.CancelAfterFailure
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.duration.*

class UpdateAssetInfoTransactionSuite extends BaseTransactionSuite with CancelAfterFailure with TableDrivenPropertyChecks {
  import UpdateAssetInfoTransactionSuite.*
  val updateInterval = 2
  override protected def nodeConfigs: Seq[Config] =
    Seq(
      configWithUpdateIntervalSetting(updateInterval).withFallback(Miners.head),
      configWithUpdateIntervalSetting(updateInterval).withFallback(NotMiner)
    )

  private def issuer    = firstKeyPair
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
    val script = ScriptCompiler.compile(testDapp, ScriptEstimatorV3.latest).explicitGet()._1.bytes().base64
    sender.setScript(dApp, Some(script), waitForTx = true)
  }

  test("DApp can read asset info") {
    sender.invokeScript(
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
    val res = sender.getDataByKey(dApp.toAddress.toString, "isAssetInfoCorrect")
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
      .as[JsObject] ++ Json.obj("type" -> TransactionType.UpdateAssetInfo.id)

    val fee = sender.calculateFee(txJson)
    fee.feeAmount shouldBe 1e5.toLong
    fee.feeAssetId shouldBe None
  }

  var updateAssetInfoTxHeight: Int = -1

  test("able to update name/description of issued asset") {
    val nextTerm = sender.transactionInfo[TransactionInfo](assetId).height + updateInterval + 1
    nodes.waitForHeight(nextTerm)
    val issuerBalance       = sender.balanceDetails(issuer.publicKey.toAddress.toString)
    val updateAssetInfoTxId = notMiner.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee)._1.id
    checkUpdateAssetInfoTx(notMiner.utx().head, "updatedName", "updatedDescription")
    miner.waitForTransaction(updateAssetInfoTxId)
    updateAssetInfoTxHeight = sender.transactionInfo[TransactionInfo](updateAssetInfoTxId).height
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

  test("DApp can read updated asset info") {
    sender.invokeScript(
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
    val res = sender.getDataByKey(dApp.toAddress.toString, "isAssetInfoCorrect")
    res.value shouldBe true
  }

  test("not able to update name/description more than once within interval") {
    val nextTermEnd = updateAssetInfoTxHeight + updateInterval
    assertApiError(sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include(
        s"Can't update info of asset with id=$assetId before $nextTermEnd block, current height=${sender.height}, minUpdateInfoInterval=$updateInterval"
      )
    }
    sender.waitForHeight(nextTermEnd - 1)

    assertApiError(sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message should include(
        s"Can't update info of asset with id=$assetId before $nextTermEnd block, current height=${sender.height}, minUpdateInfoInterval=$updateInterval"
      )
    }
  }

  var secondUpdateInfoHeight = 0

  test("able to update info of other asset after updating info of first asset") {
    nodes.waitForHeightArise()
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
    val assetId     = sender.broadcastIssue(issuer, "asset", "description", 1, 0, false, script = None, waitForTx = true).id
    val issueHeight = sender.transactionInfo[TransactionInfo](assetId).height
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

  test("can update asset only with name which have valid length") {
    val invalidNames = Seq(
      "",
      "a" * (MinAssetNameLength - 1),
      "a" * (MaxAssetNameLength + 1),
      "~!|#$%^&*()_+=\";:/?><|\\][{}"
    )
    val validNames = Seq("a" * MinAssetNameLength, "a" * MaxAssetNameLength)
    invalidNames.foreach { name =>
      assertApiError(
        sender.updateAssetInfo(issuer, assetId, name, "updatedDescription", minFee),
        InvalidName
      )
    }
    validNames.foreach { name =>
      sender.waitForHeight(sender.height + updateInterval + 1, 3.minutes)
      val (tx, _) = sender.updateAssetInfo(issuer, assetId, name, "updatedDescription", minFee)

      nodes.waitForHeightAriseAndTxPresent(tx.id)
      nodes.foreach(_.assetsDetails(assetId).name shouldBe name)
    }
  }

  test("can update asset only with description which have valid length") {
    val invalidDescs = Seq("a" * (MaxAssetDescriptionLength + 1))
    val validDescs   = Seq("", "a" * MaxAssetDescriptionLength)
    invalidDescs.foreach { desc =>
      assertApiError(
        sender.updateAssetInfo(issuer, assetId, "updatedName", desc, minFee),
        TooBigArrayAllocation
      )
    }
    validDescs.foreach { desc =>
      sender.waitForHeight(sender.height + updateInterval + 1, 3.minutes)
      val (tx, _) = sender.updateAssetInfo(issuer, assetId, "updatedName", desc, minFee)

      nodes.waitForHeightAriseAndTxPresent(tx.id)
      nodes.foreach(_.assetsDetails(assetId).description shouldBe desc)
    }
  }

  test("not able to update asset info without paying enough fee") {
    assertApiError(sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee - 1)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message shouldBe s"State check failed. Reason: Fee for UpdateAssetInfoTransaction (${minFee - 1} in WAVES) does not exceed minimal value of $minFee WAVES."
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

  test("asset script can read asset info") {
    val scriptTextT = s"""true""".stripMargin
    val scriptT     = TestCompiler.DefaultVersion.compileAsset(scriptTextT).bytes().base64

    val smartAssetId1 =
      sender.broadcastIssue(issuer, "smartAsset", "description", someAssetAmount, 8, reissuable = true, script = Some(scriptT), waitForTx = true).id
    val scriptText1 = s"""
                         |{-# STDLIB_VERSION 4 #-}
                         |{-# CONTENT_TYPE EXPRESSION #-}
                         |{-# SCRIPT_TYPE ASSET #-}

                         |match assetInfo(fromBase58String("$smartAssetId1")) {
                         |case a:Asset =>
                         | a.name == "smartAsset" &&
                         | this.name == "smartAsset" &&
                         | a.description == "description" &&
                         | this.description == "description" &&
                         | a.quantity == $someAssetAmount &&
                         | a.quantity == $someAssetAmount &&
                         | this.id == fromBase58String("$smartAssetId1") &&
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
    val script1 = TestCompiler(V4).compileAsset(scriptText1).bytes().base64
    sender.setAssetScript(smartAssetId1, issuer, setAssetScriptFee, Some(script1), waitForTx = true)

    sender.burn(issuer, smartAssetId1, 1, minFee + 2 * smartFee, waitForTx = true)
  }

  test("check increased fee for smart sender/asset") {
    val scriptText = s"""true""".stripMargin
    val script     = TestCompiler.DefaultVersion.compileAsset(scriptText).bytes().base64
    val smartAssetId =
      sender.broadcastIssue(issuer, "smartAsset", "description", someAssetAmount, 8, reissuable = true, script = Some(script), waitForTx = true).id
    sender.waitForHeight(sender.height + updateInterval + 1, 3.minutes)
    assertApiError(sender.updateAssetInfo(issuer, smartAssetId, "updatedName", "updatedDescription", minFee + smartFee - 1)) { error =>
      error.id shouldBe StateCheckFailed.Id
      error.message shouldBe s"State check failed. Reason: Transaction involves 1 scripted assets. Requires $smartFee extra fee." +
        s" Fee for UpdateAssetInfoTransaction (${smartMinFee - 1} in WAVES) does not exceed minimal value of $smartMinFee WAVES."
    }
    sender.setScript(issuer, Some(script), waitForTx = true)
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
    sender.reissue(issuer, assetId, 100, reissuable = true, version = TxVersion.V2, waitForTx = true)
    sender.updateAssetInfo(issuer, assetId, "afterReissue", "asset after reissue", waitForTx = true)
    sender.assetsDetails(assetId).name shouldBe "afterReissue"
    sender.assetsDetails(assetId).description shouldBe "asset after reissue"

    sender.waitForHeight(sender.height + updateInterval + 1, 2.minutes)
    sender.burn(issuer, assetId, 100, version = TxVersion.V2, fee = smartMinFee, waitForTx = true)
    sender.updateAssetInfo(issuer, assetId, "afterBurn", "asset after burn", waitForTx = true)
    sender.assetsDetails(assetId).name shouldBe "afterBurn"
    sender.assetsDetails(assetId).description shouldBe "asset after burn"

    sender.waitForHeight(sender.height + updateInterval + 1, 2.minutes)
    sender.setAssetScript(
      smartAssetId,
      issuer,
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
