package com.wavesplatform.transaction.smart

import com.wavesplatform.api.common.TransactionMeta
import com.wavesplatform.db.WithDomain
import com.wavesplatform.test.FlatSpec
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.transaction.utils.EthTxGenerator
import com.wavesplatform.utils.{EthHelpers, JsonMatchers}
import play.api.libs.json.Json

class EthereumTransactionStateChangesSpec extends FlatSpec with WithDomain with EthHelpers with JsonMatchers {
  "Ethereum invoke" should "work correctly with complexity > 1000" in withDomain(DomainPresets.RideV6) { d =>
    val dApp       = TxHelpers.signer(10)
    val nestedDApp = TxHelpers.signer(11)

    d.helpers.creditWavesToDefaultSigner()
    d.helpers.creditWavesFromDefaultSigner(dApp.toAddress, 1_000_000)
    d.helpers.creditWavesFromDefaultSigner(nestedDApp.toAddress, 1_000_000)

    d.helpers.setScript(
      dApp,
      TxHelpers.scriptV5(s"""@Callable(i)
        |func deposit() = {
        |  strict res1 = invoke(Address(base58'${nestedDApp.toAddress}'), "test", nil, [AttachedPayment(unit, 100)])
        |  [StringEntry("test", "foo")]
        |}""".stripMargin)
    )

    d.helpers.setScript(
      nestedDApp,
      TxHelpers.scriptV5(s"""@Callable(i)
         |func test() = {
         |  if ((${(1 to 15).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")}) || true) then throw("err")
         |  else [StringEntry("test1", "bar")]
         |}""".stripMargin)
    )

    val invoke = EthTxGenerator.generateEthInvoke(
      TxHelpers.defaultEthSigner,
      dApp.toAddress,
      "deposit",
      Nil,
      Seq(InvokeScriptTransaction.Payment(100, Waves))
    )

    d.appendBlock(invoke)
    d.makeStateHard()

    d.commonApi.transactions.transactionById(invoke.id()) match {
      case Some(meta: TransactionMeta.Ethereum) =>
        assert(!meta.succeeded, "should fail")
        Json.toJson(meta.invokeScriptResult) should matchJson(
          """
            |{
            |  "data" : [ ],
            |  "transfers" : [ ],
            |  "issues" : [ ],
            |  "reissues" : [ ],
            |  "burns" : [ ],
            |  "sponsorFees" : [ ],
            |  "leases" : [ ],
            |  "leaseCancels" : [ ],
            |  "invokes" : [ {
            |    "dApp" : "3N1aSGxVmMSUQbnporHpeX5X34X7Gec6kg3",
            |    "call" : {
            |      "function" : "test",
            |      "args" : [ ]
            |    },
            |    "payment" : [ {
            |      "assetId" : null,
            |      "amount" : 100
            |    } ],
            |    "stateChanges" : {
            |      "data" : [ ],
            |      "transfers" : [ ],
            |      "issues" : [ ],
            |      "reissues" : [ ],
            |      "burns" : [ ],
            |      "sponsorFees" : [ ],
            |      "leases" : [ ],
            |      "leaseCancels" : [ ],
            |      "invokes" : [ ],
            |      "error" : {
            |        "code" : 1,
            |        "text" : "err"
            |      }
            |    }
            |  } ],
            |  "error" : {
            |    "code" : 1,
            |    "text" : "FailedTransactionError(code = 1, error = err, log =)"
            |  }
            |}
            |""".stripMargin)

      case _ => ???
    }
  }
}
