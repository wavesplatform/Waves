package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.api.http.ApiError.ScriptExecutionError
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync.{issueFee, smartMinFee}
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class TypeEntrySuite extends BaseTransactionSuite {
  private def firstDApp = firstKeyPair

  private def secondDApp = secondKeyPair

  private def caller = thirdKeyPair

  private val base58String = "49ReVPc83oQRqoWuuTkBC"
  private var firstAssetId = ""

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    val smartAssetScript = ScriptCompiler(
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |{-# SCRIPT_TYPE ASSET #-}
         |
         |getBinaryValue(addressFromStringValue(""), "bin") == fromBase58String("$base58String").value()
         |  && getIntegerValue(addressFromStringValue(""), "int") == 1
         |  && getBooleanValue(addressFromStringValue(""), "bool") == true
         |  && getStringValue(addressFromStringValue(""), "str") == "string"
         """.stripMargin,
      isAssetScript = true,
      ScriptEstimatorV3.latest
    ).explicitGet()._1.bytes().base64

    firstAssetId = sender.issue(firstDApp, fee = issueFee, script = Some(smartAssetScript), waitForTx = true).id

    val dAppScript = ScriptCompiler(
      s"""
         |{-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE DAPP #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |let binary = fromBase58String("$base58String").value()
         |let boolean = true
         |let integer = 1
         |let string = "string"
         |
         |@Callable(i)
         |func writeEntries() = {
         |  [
         |    BinaryEntry("bin", binary),
         |    BooleanEntry("bool", boolean),
         |    IntegerEntry("int", integer),
         |    StringEntry("str", string)
         |  ]
         |}
         |
         |@Callable(i)
         |func writeDeleteEntries() = {
         |  [
         |    BinaryEntry("bin", binary),
         |    BooleanEntry("bool", boolean),
         |    IntegerEntry("int", integer),
         |    StringEntry("str", string),
         |    DeleteEntry("bin"),
         |    DeleteEntry("bool"),
         |    DeleteEntry("int"),
         |    DeleteEntry("str"),
         |    Burn(fromBase58String("$firstAssetId"), 1000)
         |  ]
         |}
         |
         |
         |@Callable(i)
         |func deleteEntries() = {
         |  [
         |    DeleteEntry("bin"),
         |    DeleteEntry("bool"),
         |    DeleteEntry("int"),
         |    DeleteEntry("str"),
         |    Burn(fromBase58String("$firstAssetId"), 1000)
         |  ]
         |}
         |
         |@Callable(i)
         |func checkEntries() = {
         |  [BooleanEntry("check",
         |   getBinaryValue(this, "bin") == binary
         |  && getIntegerValue(this, "int") == integer
         |  && getBooleanValue(this, "bool") == boolean
         |  && getStringValue(this, "str") == string)]
         |}
         |
         """.stripMargin,
      isAssetScript = false,
      ScriptEstimatorV3.latest
    ).explicitGet()._1.bytes().base64

    val accountScript = ScriptCompiler(
      s"""
         {-# STDLIB_VERSION 4 #-}
         |{-# CONTENT_TYPE EXPRESSION #-}
         |{-# SCRIPT_TYPE ACCOUNT #-}
         |
         |getBinaryValue(addressFromStringValue("${firstDApp.toAddress.toString}"), "bin") == fromBase58String("$base58String").value()
         |  && getIntegerValue(addressFromStringValue("${firstDApp.toAddress.toString}"), "int") == 1
         |  && getBooleanValue(addressFromStringValue("${firstDApp.toAddress.toString}"), "bool") == true
         |  && getStringValue(addressFromStringValue("${firstDApp.toAddress.toString}"), "str") == "string"
         |
         """.stripMargin,
      isAssetScript = false,
      ScriptEstimatorV3.latest
    ).explicitGet()._1.bytes().base64

    sender.setScript(firstDApp, Some(dAppScript), waitForTx = true)
    sender.setScript(secondDApp, Some(accountScript), waitForTx = true)
  }

  test("check dApp getEntry after delete") {
    sender.invokeScript(
      caller,
      firstDApp.toAddress.toString,
      func = Some("writeEntries"),
      fee = issueFee,
      waitForTx = true
    )
    assertApiError(
      sender.invokeScript(
        caller,
        firstDApp.toAddress.toString,
        func = Some("deleteEntries"),
        fee = issueFee
      )
    ) { err =>
      err.message should include regex "called on unit"
      err.id shouldBe ScriptExecutionError.Id
    }
    assertApiError(
      sender.invokeScript(
        caller,
        firstDApp.toAddress.toString,
        func = Some("writeDeleteEntries"),
        fee = issueFee
      )
    ) { err =>
      err.message should include regex "called on unit"
      err.id shouldBe ScriptExecutionError.Id
    }
  }

  test("check dApp getEntry") {
    sender.invokeScript(
      caller,
      firstDApp.toAddress.toString,
      func = Some("writeEntries"),
      fee = smartMinFee,
      waitForTx = true
    )
    sender.invokeScript(
      caller,
      firstDApp.toAddress.toString,
      func = Some("checkEntries"),
      fee = smartMinFee,
      waitForTx = true
    )
    sender.getDataByKey(firstDApp.toAddress.toString, "str").value shouldBe "string"
    s"${sender.getDataByKey(firstDApp.toAddress.toString, "bin").value}" shouldBe base58String
    sender.getDataByKey(firstDApp.toAddress.toString, "bool").value shouldBe true
    sender.getDataByKey(firstDApp.toAddress.toString, "int").value shouldBe 1
    sender.getDataByKey(firstDApp.toAddress.toString, "check").value shouldBe true
  }

  test("check account getEntry") {
    sender.transfer(secondDApp, secondDApp.toAddress.toString, 1000, smartMinFee, waitForTx = true)
  }
}
