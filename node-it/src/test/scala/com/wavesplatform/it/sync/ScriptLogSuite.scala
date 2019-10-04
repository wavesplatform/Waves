package com.wavesplatform.it.sync

import com.wavesplatform.api.http.ApiError.TransactionNotAllowedByAccountScript
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils._
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2
import com.wavesplatform.state.BinaryDataEntry
import com.wavesplatform.transaction.DataTransaction
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class ScriptLogSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val smart = pkByAddress(firstAddress)

  val ENOUGH_FEE: Long = 12900000L

  val scriptSrc: String =
    s"""
       |let self = Address(base58'$firstAddress')
       |
      |match tx {
       |	case dtx: DataTransaction =>
       |		let v00 = extract(getBinary(self, "k0"))
       |		let v01 = extract(getBinary(self, "k1"))
       |		let v02 = extract(getBinary(self, "k2"))
       |		let v03 = extract(getBinary(self, "k3"))
       |
       |		let pk = extract(getBinary(dtx.data, "pk"))
       |		let sig = extract(getBinary(dtx.data, "sig"))
       |   # let msgPart1 = v00 + v01
       |   # let msgPart2 = v00 + v02
       |   # let msgPart3 = v00 + v03
       |   # let msgPart4 = v01 + v02
       |   # let msgPart5 = v01 + v03
       |   # let msgPart6 = v02 + v03
       |
       |	 # sigVerify(msgPart1, sig, pk) &&
       |   # sigVerify(msgPart2, sig, pk) &&
       |   # sigVerify(msgPart3, sig, pk) &&
       |   # sigVerify(msgPart4, sig, pk) &&
       |   # sigVerify(msgPart5, sig, pk) &&
       |   # sigVerify(msgPart6, sig, pk)
       |   sigVerify(v00, sig, pk) &&
       |   sigVerify(v01, sig, pk) &&
       |   sigVerify(v02, sig, pk) &&
       |   sigVerify(v03, sig, pk)
       |
       |	case _ => false
       |}
    """.stripMargin

  test("set contract, put a lot of data, invoke test") {

    val data =
      ((0 until 4) map { i =>
        val bytes = new Array[Byte](Short.MaxValue - 1)
        Random.nextBytes(bytes)
        BinaryDataEntry(s"k$i", ByteStr(bytes))
      }).toList

    val initialData = DataTransaction
      .selfSigned(
        smart,
        data,
        ENOUGH_FEE,
        System.currentTimeMillis()
      )
      .explicitGet()

    val dtx1_id = sender.signedBroadcast(initialData.json()).id

    nodes.waitForHeightAriseAndTxPresent(dtx1_id)

    val script = ScriptCompiler(scriptSrc, isAssetScript = false, ScriptEstimatorV2).explicitGet()._1
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(smart, Some(script), setScriptFee, System.currentTimeMillis())
      .explicitGet()

    val sstx = sender.signedBroadcast(setScriptTransaction.json()).id

    nodes.waitForHeightAriseAndTxPresent(sstx)

    val signature = new Array[Byte](64)

    Random.nextBytes(signature)

    def mkInvData() =
      DataTransaction
        .selfSigned(
          smart,
          List(
            BinaryDataEntry("pk", ByteStr(smart.publicKey)),
            BinaryDataEntry("sig", ByteStr(signature)),
          ),
          ENOUGH_FEE,
          System.currentTimeMillis()
        )
        .explicitGet()

    assertApiErrorRaised(sender.signedBroadcast(mkInvData().json()))

    def async = com.wavesplatform.it.api.AsyncHttpApi.NodeAsyncHttpApi _

    val requests =
      (0 to 100)
        .map { _ =>
          async(sender).expectSignedBroadcastRejected(mkInvData().json())
        }

    val result = Future
      .sequence(requests)
      .map {
        _.forall(_ == TransactionNotAllowedByAccountScript.Id)
      }

    Await.result(result, 1.minute) shouldBe true
  }
}
