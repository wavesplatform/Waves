package com.wavesplatform.it.sync.smartcontract.smartasset

import com.wavesplatform.it.api.SyncHttpApi.*
import com.wavesplatform.it.sync.*
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.TestCompiler
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
    )
  ) { tx =>
    test(s"Smart Asset script should not support $tx") {
      try {
        sender.issue(
          firstKeyPair,
          "MyAsset",
          "Test Asset",
          someAssetAmount,
          0,
          reissuable = true,
          issueFee,
          2,
          Some(
            TestCompiler.DefaultVersion
              .compileAsset(
                s"""
                   |match tx {
                   |  case _: $tx => true
                   |  case _ => true
                   |}""".stripMargin
              )
              .bytes()
              .base64
          ),
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
        firstKeyPair,
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
    assertBadRequestAndMessage(
      sender.sponsorAsset(firstKeyPair, assetId, 100, sponsorReducedFee + smartFee),
      "State check failed. Reason: Sponsorship smart assets is disabled."
    )

  }

}
