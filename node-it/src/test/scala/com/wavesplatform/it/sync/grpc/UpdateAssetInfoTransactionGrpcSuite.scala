package com.wavesplatform.it.sync.grpc

import com.typesafe.config.{Config, ConfigFactory}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.NodeConfigs.Miners
import com.wavesplatform.it.api.SyncGrpcApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.protobuf.transaction.PBTransactions
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import io.grpc.Status.Code
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.concurrent.duration._
import scala.util.Random

class UpdateAssetInfoTransactionGrpcSuite extends GrpcBaseTransactionSuite with TableDrivenPropertyChecks {
  import UpdateAssetInfoTransactionGrpcSuite._
  val updateInterval                              = 2
  override protected def nodeConfigs: Seq[Config] = Seq(configWithUpdateIntervalSetting(updateInterval).withFallback(Miners.head))

  val issuer      = firstAcc
  val nonIssuer   = secondAcc
  var assetId     = ""
  var issueHeight = 0

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    assetId = PBTransactions
      .vanilla(
        sender.broadcastIssue(
          issuer,
          "asset",
          someAssetAmount,
          8,
          reissuable = true,
          script = Right(None),
          fee = issueFee,
          description = "description",
          version = 1,
          waitForTx = true
        )
      )
      .explicitGet()
      .id()
      .toString
    issueHeight = sender.height
  }

  test("able to update name/description of issued asset") {
    val nextTerm = issueHeight + updateInterval + 1
    sender.waitForHeight(nextTerm)
    val updateAssetInfoTxId =
      PBTransactions
        .vanilla(
          sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee)
        )
        .explicitGet()
        .id()
        .toString
    sender.waitForTransaction(updateAssetInfoTxId)

    sender.assetInfo(assetId).name shouldBe "updatedName"
    sender.assetInfo(assetId).description shouldBe "updatedDescription"
  }

  test("not able to update name/description more than once within interval") {
    val nextTermEnd = issueHeight + 2 * updateInterval
    assertGrpcError(
      sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee),
      s"Can't update info of asset with id=$assetId",
      Code.INVALID_ARGUMENT
    )
    sender.waitForHeight(nextTermEnd)

    assertGrpcError(
      sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee),
      s"Can't update info of asset with id=$assetId",
      Code.INVALID_ARGUMENT
    )
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
      sender.waitForHeight(sender.height + 3, 2.minutes)
      assertGrpcError(
        sender.updateAssetInfo(issuer, assetId, assetName, "updatedDescription", minFee),
        "invalid name",
        Code.INVALID_ARGUMENT
      )
    }
  }

  test(s"not able to set too big description") {
    val tooBigDescription = Random.nextString(1001)
    assertGrpcError(
      sender.updateAssetInfo(issuer, assetId, "updatedName", tooBigDescription, minFee),
      "Too big sequences requested",
      Code.INVALID_ARGUMENT
    )
  }

  test("not able to update asset info without paying enough fee") {
    assertGrpcError(
      sender.updateAssetInfo(issuer, assetId, "updatedName", "updatedDescription", minFee - 1),
      s"does not exceed minimal value of $minFee WAVES",
      Code.INVALID_ARGUMENT
    )
  }

  test("not able to update info of not-issued asset") {
    val notIssuedAssetId = "BzARFPgBqWFu6MHGxwkPVKmaYAzyShu495Ehsgru72Wz"
    assertGrpcError(
      sender.updateAssetInfo(issuer, notIssuedAssetId, "updatedName", "updatedDescription", minFee),
      "Referenced assetId not found",
      Code.INVALID_ARGUMENT
    )
  }

  test("non-issuer cannot update asset info") {
    assertGrpcError(
      sender.updateAssetInfo(nonIssuer, assetId, "updatedName", "updatedDescription", minFee),
      "Asset was issued by other address",
      Code.INVALID_ARGUMENT
    )
  }

  test("check increased fee for smart sender/asset") {
    val scriptText = s"""true""".stripMargin
    val script     = ScriptCompiler(scriptText, isAssetScript = true, ScriptEstimatorV2).explicitGet()._1
    val smartAssetId =
      PBTransactions
        .vanilla(
          sender.broadcastIssue(
            issuer,
            "smartAsset",
            someAssetAmount,
            8,
            reissuable = true,
            description = "description",
            fee = issueFee,
            script = Right(Some(script)),
            waitForTx = true
          )
        )
        .explicitGet()
        .id()
        .toString
    sender.waitForHeight(sender.height + updateInterval + 1, 3.minutes)
    assertGrpcError(
      sender.updateAssetInfo(issuer, smartAssetId, "updatedName", "updatedDescription", minFee + smartFee - 1),
      s"State check failed. Reason: Transaction involves 1 scripted assets. Requires $smartFee extra fee.",
      Code.INVALID_ARGUMENT
    )
    sender.setScript(issuer, Right(Some(script)), fee = setScriptFee, waitForTx = true)
    assertGrpcError(
      sender.updateAssetInfo(issuer, smartAssetId, "updatedName", "updatedDescription", minFee + 2 * smartFee - 1),
      s"State check failed. Reason: Transaction sent from smart account. Requires $smartFee extra fee.",
      Code.INVALID_ARGUMENT
    )

    sender.updateAssetInfo(issuer, smartAssetId, "updatedName", "updatedDescription", minFee + 2 * smartFee, waitForTx = true)
  }

}

object UpdateAssetInfoTransactionGrpcSuite {
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
