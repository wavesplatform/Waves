package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.state._
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

import scala.util.Random

class InvokeListForCallable extends BaseTransactionSuite with CancelAfterFailure {
  private val dApp   = firstAddress
  private val caller = secondAddress

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
      |
      |@Callable(inv)
      |func f2(a:List[Boolean], idx: Int) = [
      |  BooleanEntry("a", a[idx])
      |]
      |
      |@Callable(inv)
      |func checksize(a:List[Boolean|Int|ByteVector|String]) = {
      |  func checkType(acc: List[IntegerEntry|StringEntry|BinaryEntry|BooleanEntry], arg: Int|String|ByteVector|Boolean) = {
      |    match arg {
      |      case x: Int => acc :+ IntegerEntry("a", x)
      |      case y: String => acc :+ StringEntry("b", y)
      |      case z: ByteVector => acc :+ BinaryEntry("c", z)
      |      case w: Boolean => acc :+ BooleanEntry("y", w)
      |      case _ => throw("unknown type")
      |    }
      |  }
      |  IntegerEntry("listsize", a.size()) :: FOLD<4>(a, [], checkType)
      |}
      """.stripMargin
    val script = ScriptCompiler.compile(source, ScriptEstimatorV2).explicitGet()._1.bytes().base64
    sender.setScript(dApp, Some(script), setScriptFee, waitForTx = true)
  }

  test("check list for all data types except union. Write first element of list of each type to acc data") {
    val rndString = Random.nextString(10)
    val intList   = ARR(IndexedSeq(CONST_LONG(Long.MaxValue)), limited = false).explicitGet()
    val strList   = ARR(IndexedSeq(CONST_STRING(rndString).explicitGet()), limited = false).explicitGet()
    val byteList  = ARR(IndexedSeq(CONST_BYTESTR(ByteStr(rndString.getBytes())).explicitGet()), limited = false).explicitGet()
    val boolList  = ARR(IndexedSeq(CONST_BOOLEAN(true)), limited = false).explicitGet()

    sender
      .invokeScript(
        caller,
        dApp,
        Some("f"),
        args = List(intList, strList, byteList, boolList),
        waitForTx = true
      )

    sender.getData(dApp, "a") shouldBe List(IntegerDataEntry("a", Long.MaxValue))
    sender.getData(dApp, "b") shouldBe List(StringDataEntry("b", rndString))
    sender.getData(dApp, "c") shouldBe List(BinaryDataEntry("c", ByteStr(rndString.getBytes)))
    sender.getData(dApp, "y") shouldBe List(BooleanDataEntry("y", true))
  }

  test("List can contain union data type") {
    val rndString = Random.nextString(10)
    val intEl   = CONST_LONG(Long.MaxValue)
    val strEl   = CONST_STRING(rndString).explicitGet()
    val byteEl  = CONST_BYTESTR(ByteStr(rndString.getBytes())).explicitGet()
    val boolEl  = CONST_BOOLEAN(true)

    sender
      .invokeScript(
        caller,
        dApp,
        Some("checksize"),
        args = List(ARR(IndexedSeq(intEl, strEl, boolEl, byteEl), limited = false).explicitGet()),
        waitForTx = true
      )

    sender.getData(dApp, "a") shouldBe List(IntegerDataEntry("a", Long.MaxValue))
    sender.getData(dApp, "b") shouldBe List(StringDataEntry("b", rndString))
    sender.getData(dApp, "c") shouldBe List(BinaryDataEntry("c", ByteStr(rndString.getBytes)))
    sender.getData(dApp, "y") shouldBe List(BooleanDataEntry("y", true))
    sender.getData(dApp, "listsize") shouldBe List(IntegerDataEntry("a", 4))
  }


  test("can set different data types from first list el") {
    val rndString = Random.nextString(10)
    val intList   = ARR(IndexedSeq(CONST_LONG(Long.MaxValue)), limited = false).explicitGet()
    val strList   = ARR(IndexedSeq(CONST_STRING(rndString).explicitGet()), limited = false).explicitGet()
    val byteList  = ARR(IndexedSeq(CONST_BYTESTR(ByteStr(rndString.getBytes())).explicitGet()), limited = false).explicitGet()
    val boolList  = ARR(IndexedSeq(CONST_BOOLEAN(true)), limited = false).explicitGet()

    sender
      .invokeScript(
        caller,
        dApp,
        Some("f"),
        args = List(intList, strList, byteList, boolList),
        waitForTx = true
      )

    sender.getData(dApp, "a") shouldBe List(IntegerDataEntry("a", Long.MaxValue))
    sender.getData(dApp, "b") shouldBe List(StringDataEntry("b", rndString))
    sender.getData(dApp, "c") shouldBe List(BinaryDataEntry("c", ByteStr(rndString.getBytes)))
    sender.getData(dApp, "y") shouldBe List(BooleanDataEntry("y", true))
  }

  test("error if list size more than 1000") {
    val strList = ARR(genArrOfBoolean(1001), limited = false).explicitGet()
    assertApiError(
      sender
        .invokeScript(
          caller,
          dApp,
          Some("f2"),
          args = List(strList, CONST_LONG(0))
        )
    ) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "List size should not exceed 1000"
    }
  }

  test("try to get non-existing element by index") {
    val strList = ARR(genArrOfBoolean(1000), limited = false).explicitGet()
    assertApiError(
      sender
        .invokeScript(
          caller,
          dApp,
          Some("f2"),
          args = List(strList, CONST_LONG(1000)),
          waitForTx = true
        )
    ) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "Error while executing account-script: Index 1000 out of bounds for length 1000"
    }
  }

  test("try to get element by negative index") {
    val strList = ARR(genArrOfBoolean(5), limited = false).explicitGet()
    assertApiError(
      sender
        .invokeScript(
          caller,
          dApp,
          Some("f2"),
          args = List(strList, CONST_LONG(-1)),
          waitForTx = true
        )
    ) { error =>
      error.statusCode shouldBe 400
      error.message shouldBe "Error while executing account-script: Index -1 out of bounds for length 5"
    }
  }

  def genArrOfBoolean(size: Integer): IndexedSeq[CONST_BOOLEAN] = {
    IndexedSeq.fill(size)(CONST_BOOLEAN(true))
  }

}
