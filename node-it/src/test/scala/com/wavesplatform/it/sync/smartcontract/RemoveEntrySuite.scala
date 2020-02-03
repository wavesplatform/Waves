package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.CONST_STRING
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state.StringDataEntry
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class RemoveEntrySuite extends BaseTransactionSuite with CancelAfterFailure {

  test("Entry could be removed from account storage") {
    val k       = "someKey"
    val v       = "someValue"
    val address = createDapp()

    invokeScript(address, "write", k, v)
    sender.getData(address) should have size 1
    sender.getDataByKey(address, k) shouldBe StringDataEntry(k, v)

    invokeScript(address, "remove", k)
    sender.getData(address) should have size 0
  }

  test("Removing nonexistent entry should not produce error") {
    val address = createDapp()

    invokeScript(address, "remove", "nonexistent-key")
  }

  def createDapp(): String = {
    val address = sender.createAddress()
    val compiledScript = ScriptCompiler
      .compile(
        s"""
           |{-# STDLIB_VERSION 4 #-}
           |{-# SCRIPT_TYPE ACCOUNT #-}
           |{-# CONTENT_TYPE DAPP #-}
           |
           |@Callable(i)
           |func write(k: String, v: String) = {
           |  [StringEntry(k, v)]
           |}
           |
           |@Callable(i)
           |func remove(k: String) = {
           |  [DeleteEntry(k)]
           |}
       """.stripMargin,
        ScriptEstimatorV2
      )
      .explicitGet()
      ._1

    sender.transfer(pkByAddress(firstAddress).stringRepr, address, 10.waves, minFee, waitForTx = true)

    nodes.waitForHeightAriseAndTxPresent(
      sender
        .signedBroadcast(
          SetScriptTransaction
            .selfSigned(1.toByte, pkByAddress(address), Some(compiledScript), setScriptFee, System.currentTimeMillis())
            .explicitGet()
            .json
            .value
        )
        .id
    )

    return address
  }

  def invokeScript(address: String, function: String, key: String = "", value: String = ""): Unit = {
    val args = function match {
      case "write"  => List(CONST_STRING(key).explicitGet(), CONST_STRING(value).explicitGet())
      case "remove" => List(CONST_STRING(key).explicitGet())
      case _        => List.empty
    }

    nodes.waitForHeightAriseAndTxPresent(
      sender
        .invokeScript(
          address,
          address,
          fee = smartMinFee + smartFee,
          waitForTx = true,
          func = Some(function),
          args = args
        )
        ._1
        .id
    )
  }

}
