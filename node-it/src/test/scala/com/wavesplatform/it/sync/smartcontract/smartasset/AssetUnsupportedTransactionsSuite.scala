package com.wavesplatform.it.sync.smartcontract.smartasset

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{someAssetAmount, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.prop.TableDrivenPropertyChecks

class AssetUnsupportedTransactionsSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  forAll(
    Table(
      "tx",
      "SponsorFeeTransaction",
      "LeaseTransaction",
      "LeaseCancelTransaction",
      "CreateAliasTransaction",
      "SetScriptTransaction",
      "DataTransaction",
      "IssueTransaction"
    )) { tx =>
    test(s"Smart Asset script should not support $tx") {
      try {
        sender.issue(
          firstAddress,
          "MyAsset",
          "Test Asset",
          someAssetAmount,
          0,
          reissuable = true,
          issueFee,
          2,
          Some(
            ScriptCompiler(
              s"""
                 |match tx {
                 |  case s : $tx => true
                 |  case _ => true
                 |}""".stripMargin,
              isAssetScript = true,
              ScriptEstimatorV2
            ).explicitGet()._1.bytes.value.base64),
          waitForTx = true
        )

        fail("ScriptCompiler didn't throw expected error")
      } catch {
        case ex: java.lang.Exception => ex.getMessage should include("Matching not exhaustive: possibleTypes are")
        case _: Throwable            => fail("ScriptCompiler works incorrect for orders with smart assets")
      }
    }
  }

  test("cannot sponsor scripted asset") {
    val assetId = sender
      .issue(
        firstAddress,
        "MyAsset",
        "Test Asset",
        someAssetAmount,
        0,
        reissuable = true,
        issueFee,
        2,
        Some(scriptBase64),
        waitForTx = true
      )
      .id
    assertBadRequestAndMessage(sender.sponsorAsset(firstAddress, assetId, 100, sponsorFee + smartFee),
                               "State check failed. Reason: Sponsorship smart assets is disabled.")

  }

}
