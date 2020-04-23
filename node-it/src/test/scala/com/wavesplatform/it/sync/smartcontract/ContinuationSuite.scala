package com.wavesplatform.it.sync.smartcontract

import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import monix.eval.Coeval
import org.scalatest.CancelAfterFailure

class ContinuationSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val dApp   = firstAddress
  private val caller = secondAddress

  private val dummyEstimator = new ScriptEstimator {
    override val version: Int = 0

    override def apply(
        declaredVals: Set[String],
        functionCosts: Map[FunctionHeader, Coeval[Long]],
        expr: Terms.EXPR
    ): Either[String, Long] = Right(1)
  }

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    val script = {
      val scriptText =
        s"""
          |{-# STDLIB_VERSION 4 #-}
          |{-# CONTENT_TYPE DAPP #-}
          |
          | @Callable(inv)
          | func foo() = {
          |  let a = ${List.fill(100)("sigVerify(base64'',base64'',base64'')").mkString("||")}
          |  [BooleanEntry("a", a), BinaryEntry("sender", inv.caller.bytes)]
          | }
          |
        """.stripMargin
      ScriptCompiler.compile(scriptText, dummyEstimator).explicitGet()._1.bytes().base64
    }
    sender.setScript(dApp, Some(script), setScriptFee, waitForTx = true).id

    val acc0ScriptInfo = sender.addressScriptInfo(dApp)
    acc0ScriptInfo.script.isEmpty shouldBe false
    acc0ScriptInfo.scriptText.isEmpty shouldBe false
    acc0ScriptInfo.script.get.startsWith("base64:") shouldBe true
  }

  test("successful continuation") {
    val invokeScriptTx = sender.invokeScript(
      caller,
      dApp,
      func = Some("foo"),
      args = Nil,
      payment = Seq(Payment(1.waves, Waves)),
      fee = 1.waves,
      version = TxVersion.V2,
      waitForTx = true
    )
    nodes.waitForHeight(nodes.height.max + 2)

    nodes.foreach { node =>
      node.getDataByKey(dApp, "a") shouldBe BooleanDataEntry("a", false)
      node.getDataByKey(dApp, "sender") shouldBe BinaryDataEntry("sender", Base58.decode(caller))
    }
  }

  test("insufficient fee") {
    lazy val invokeScriptTx = sender.invokeScript(
      caller,
      dApp,
      func = Some("foo"),
      args = Nil,
      payment = Seq(Payment(1.waves, Waves)),
      fee = invokeFee,
      version = TxVersion.V2
    )

    assertBadRequestAndMessage(
      invokeScriptTx,
      "State check failed. Reason: " +
        "Fee in WAVES for InvokeScriptTransaction (900000 in WAVES) " +
        "with 6 invocation steps " +
        "does not exceed minimal value of 3000000 WAVES."
    )
  }
}
