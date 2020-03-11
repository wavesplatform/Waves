package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.api.http.ApiError._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.compiler.Terms.{ARR, CONST_BOOLEAN, CONST_BYTESTR, CONST_LONG, CONST_STRING}
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

import scala.util.Random

class InvokeListForCallable extends BaseTransactionSuite with CancelAfterFailure {
  private val dApp   = pkByAddress(firstAddress).stringRepr
  private val caller = pkByAddress(secondAddress).stringRepr

  private var asset1: IssuedAsset = _
  private var asset2: IssuedAsset = _

  test("prerequisite: set contract and issue asset") {
    val source =
      """
      |{-# STDLIB_VERSION 4 #-}
      |{-# CONTENT_TYPE DAPP #-}
      |{-# SCRIPT_TYPE ACCOUNT #-}
      |
      |
      |@Callable(inv)
      |func f(a:List[Int], b:List[String], c:List[ByteVector], y: List[Boolean]) = [
      |  IntegerEntry("a", a[0]),
      |  StringEntry("b", b[0]),
      |  BinaryEntry("c", c[0]),
      |  BooleanEntry("y", y[0])
      |]
      """.stripMargin
    val script = ScriptCompiler.compile(source, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    sender.setScript(dApp, Some(script), setScriptFee, waitForTx = true)
  }

  test("can set different data types from first list el") {
    val rndString = Random.nextString(10)
    val intList = ARR(IndexedSeq(CONST_LONG(Long.MaxValue)))
    val strList = ARR(IndexedSeq(CONST_STRING(rndString).explicitGet()))
    val byteList = ARR(IndexedSeq(CONST_BYTESTR(rndString.getBytes()).explicitGet()))
    val boolList = ARR(IndexedSeq(CONST_BOOLEAN(true)))
    //not supported yet
    //val listOfListOfInt = ARR(IndexedSeq(ARR(IndexedSeq(CONST_LONG(Long.MaxValue)))))


    sender
      .invokeScript(
        caller,
        dApp,
        Some("f"),
        args = List(intList, strList, byteList, boolList),
        waitForTx = true
      )

    sender.getData(dApp, "a") shouldBe Long.MaxValue
    sender.getData(dApp, "b") shouldBe rndString
    sender.getData(dApp, "c") shouldBe Base58.decode(rndString)
    sender.getData(dApp, "y") shouldBe true

  }

}
