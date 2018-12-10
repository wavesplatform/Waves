package com.wavesplatform.it.sync.smartcontract.smartasset

import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{someAssetAmount, _}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.{Failure, Success, Try}

class AssetUnsupportedTransactionsSuite extends BaseTransactionSuite with TableDrivenPropertyChecks {
  var asset = ""

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    asset = sender
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
  }

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
      Try {
        sender.setAssetScript(
          asset,
          firstAddress,
          setAssetScriptFee + smartFee,
          Some(
            ScriptCompiler(
              s"""
                |match tx {
                |  case s : $tx => true
                |  case _ => true
                |}""".stripMargin,
              isAssetScript = true
            ).explicitGet()._1.bytes.value.base64)
        )
      } match {
        case Success(_) => fail("ScriptCompiler didn't throw expected error")
        case Failure(f) => f.getMessage should include("Matching not exhaustive: possibleTypes are")
      }
    }
  }

}
