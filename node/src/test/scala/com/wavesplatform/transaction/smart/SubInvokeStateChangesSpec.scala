package com.wavesplatform.transaction.smart

import com.wavesplatform.account.Address
import com.wavesplatform.db.WithDomain
import com.wavesplatform.it.util.DoubleExt
import com.wavesplatform.lang.directives.values.StdLibVersion
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.transaction.TxHelpers
import org.scalatest.{FlatSpec, Matchers}
import play.api.libs.json.Json

class SubInvokeStateChangesSpec extends FlatSpec with Matchers with WithDomain {
  val ContractFunction = "default"
  val compileV5: String => Script = TestCompiler(StdLibVersion.V5).compileContract _

  "Invoke state changes" should "include intermediary invokes" in withDomain(DomainPresets.RideV5) { d =>
    val dAppAddress = TxHelpers.signer(1)

    { // Prerequisites
      val addr2 = TxHelpers.signer(2)
      val addr3 = TxHelpers.signer(3)

      val script1 = compileV5(genScript(Some(addr2.toAddress)))
      val script2 = compileV5(genScript(Some(addr3.toAddress)))
      val script3 = compileV5(genScript(None, fail = true))

      val genesis = Seq(
        TxHelpers.genesis(TxHelpers.defaultAddress),
        TxHelpers.genesis(dAppAddress.toAddress, 1.waves),
        TxHelpers.genesis(addr2.toAddress, 1.waves),
        TxHelpers.genesis(addr3.toAddress, 1.waves)
      )
      val setScripts = Seq(
        TxHelpers.setScript(dAppAddress, script1),
        TxHelpers.setScript(addr2, script2),
        TxHelpers.setScript(addr3, script3)
      )
      d.appendBlock(genesis ++ setScripts: _*)
    }

    // Actual test
    val invoke = TxHelpers.invoke(dAppAddress.toAddress, ContractFunction)
    d.appendBlock(invoke)

    val stateChanges = d.commonApi.invokeScriptResult(invoke.id())
    val json         = Json.toJson(stateChanges)
    println(Json.prettyPrint(json))
    json shouldBe Json.parse("""""")
  }

  def genScript(otherDApp: Option[Address], fail: Boolean = false): String =
    s"""
       |{-# STDLIB_VERSION 5       #-}
       |{-# CONTENT_TYPE   DAPP    #-}
       |{-# SCRIPT_TYPE    ACCOUNT #-}
       |
       |@Callable(i)
       |func $ContractFunction() = {
       |  ${otherDApp.fold("")(address => s"""strict r = Invoke(Address(base58'$address'), "$ContractFunction", [], [])""")}
       |  if ($fail && !(${(1 to 10).map(_ => "sigVerify(base58'', base58'', base58'')").mkString(" || ")})) then throw("boom") else []
       |}""".stripMargin
}
