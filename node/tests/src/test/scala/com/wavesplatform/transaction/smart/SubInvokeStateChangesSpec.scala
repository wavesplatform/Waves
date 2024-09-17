package com.wavesplatform.transaction.smart

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.lang.directives.values.V5
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.test.*
import com.wavesplatform.transaction.TxHelpers
import com.wavesplatform.utils.JsonMatchers
import play.api.libs.json.Json

class SubInvokeStateChangesSpec extends FlatSpec with WithDomain with JsonMatchers {
  val ContractFunction            = "default"
  val compileV5: String => Script = TestCompiler(V5).compileContract(_)

  "Invoke state changes" should "include intermediate invokes" in {
    // Root DApp, calls addr2s and addr2f
    val dAppAddress = TxHelpers.signer(1)

    // Success chain
    val addr2s = TxHelpers.signer(3) // Calls addr3s
    val addr3s = TxHelpers.signer(5) // Finishes successfully

    // Failed chain
    val addr2f = TxHelpers.signer(2) // Calls addr3f
    val addr3f = TxHelpers.signer(4) // Fails

    val balances = Seq(dAppAddress, addr2f, addr3f, addr2s, addr3s).map(acc => AddrWithBalance(acc.toAddress, 1.waves)) :+
      AddrWithBalance(TxHelpers.defaultAddress)

    withDomain(DomainPresets.RideV5, balances) { d =>
      { // Prerequisites
        val script1    = compileV5(genScript(Seq(addr2s.toAddress, addr2f.toAddress)))
        val script2    = compileV5(genScript(Some(addr3f.toAddress)))
        val script3    = compileV5(genScript(None, fail = true))
        val script2alt = compileV5(genScript(Some(addr3s.toAddress)))
        val script3alt = compileV5(genScript(None))

        val setScripts = Seq(
          TxHelpers.setScript(dAppAddress, script1),
          TxHelpers.setScript(addr2f, script2),
          TxHelpers.setScript(addr3f, script3),
          TxHelpers.setScript(addr2s, script2alt),
          TxHelpers.setScript(addr3s, script3alt)
        )
        d.appendBlock(setScripts*)
      }

      // Actual test
      val invoke = TxHelpers.invoke(dAppAddress.toAddress, Some(ContractFunction))
      d.appendBlock(invoke)

      val stateChanges = d.commonApi.invokeScriptResult(invoke.id())
      val json         = Json.toJson(stateChanges)
      json should matchJson(
        """{
          |  "data" : [ ],
          |  "transfers" : [ ],
          |  "issues" : [ ],
          |  "reissues" : [ ],
          |  "burns" : [ ],
          |  "sponsorFees" : [ ],
          |  "leases" : [ ],
          |  "leaseCancels" : [ ],
          |  "invokes" : [ {
          |    "dApp" : "3N4DiVEiZHzcjEhoBx2kmoKKCH7GBZMim3L",
          |    "call" : {
          |      "function" : "default",
          |      "args" : [ ]
          |    },
          |    "payment" : [ {
          |      "assetId" : null,
          |      "amount" : 17
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
          |      "invokes" : [ {
          |        "dApp" : "3MvAdB2DFMf6unzX6czVGgco5rA24End8Jn",
          |        "call" : {
          |          "function" : "default",
          |          "args" : [ ]
          |        },
          |        "payment" : [ {
          |          "assetId" : null,
          |          "amount" : 17
          |        } ],
          |        "stateChanges" : {
          |          "data" : [ ],
          |          "transfers" : [ ],
          |          "issues" : [ ],
          |          "reissues" : [ ],
          |          "burns" : [ ],
          |          "sponsorFees" : [ ],
          |          "leases" : [ ],
          |          "leaseCancels" : [ ],
          |          "invokes" : [ ]
          |        }
          |      } ]
          |    }
          |  }, {
          |    "dApp" : "3MsY23LPQnvPZnBKpvs6YcnCvGjLVD42pSy",
          |    "call" : {
          |      "function" : "default",
          |      "args" : [ ]
          |    },
          |    "payment" : [ {
          |      "assetId" : null,
          |      "amount" : 17
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
          |      "invokes" : [ {
          |        "dApp" : "3N87Qja7rNj8z6H7nG9EYtjCXQtZLawaxyM",
          |        "call" : {
          |          "function" : "default",
          |          "args" : [ ]
          |        },
          |        "payment" : [ {
          |          "assetId" : null,
          |          "amount" : 17
          |        } ],
          |        "stateChanges" : {
          |          "data" : [ ],
          |          "transfers" : [ ],
          |          "issues" : [ ],
          |          "reissues" : [ ],
          |          "burns" : [ ],
          |          "sponsorFees" : [ ],
          |          "leases" : [ ],
          |          "leaseCancels" : [ ],
          |          "invokes" : [ ],
          |          "error" : {
          |            "code" : 1,
          |            "text" : "boom"
          |          }
          |        }
          |      } ]
          |    }
          |  } ],
          |  "error" : {
          |    "code" : 1,
          |    "text" : "boom"
          |  }
          |}
          |""".stripMargin
      )

      val allAddresses = Seq(dAppAddress, addr2s, addr3s, addr2f, addr3f).map(_.toAddress)
      for ((addr, i) <- allAddresses.zipWithIndex)
        withClue(s"Addr #${i + 1}")(d.commonApi.addressTransactions(addr).map(_.transaction) should contain(invoke))
    }
  }

  def genScript(calls: Iterable[Address], fail: Boolean = false): String =
    s"""
       |{-# STDLIB_VERSION 5       #-}
       |{-# CONTENT_TYPE   DAPP    #-}
       |{-# SCRIPT_TYPE    ACCOUNT #-}
       |
       |@Callable(i)
       |func $ContractFunction() = {
       |  ${calls.zipWithIndex
      .map { case (address, i) => s"""strict r$i = invoke(Address(base58'$address'), "$ContractFunction", [], [AttachedPayment(unit, 17)])""" }
      .mkString("\n")}
       |  if ($fail && !(${(1 to 10).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")})) then throw("boom") else []
       |}""".stripMargin
}
