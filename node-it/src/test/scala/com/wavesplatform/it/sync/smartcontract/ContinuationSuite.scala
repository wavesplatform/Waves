package com.wavesplatform.it.sync.smartcontract

import cats.Id
import cats.implicits._
import com.wavesplatform.api.http.ApiError.StateCheckFailed
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.TransactionInfo
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BYTESTR, EXPR}
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.traits.domain.{BlockInfo, Recipient, ScriptAssetInfo, Tx}
import com.wavesplatform.lang.v1.traits.{DataType, Environment}
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.{ContinuationTransaction, InvokeScriptTransaction}
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
          |  let a = ${List.fill(40)("sigVerify(base64'',base64'',base64'')").mkString("||")}
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

    nodes.waitForHeightAriseAndTxPresent(invokeScriptTx._1.id)

    sender.getDataByKey(dApp, "a") shouldBe BooleanDataEntry("a", false)
    sender.getDataByKey(dApp, "sender") shouldBe BinaryDataEntry("sender", Base58.decode(caller))
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
        "with 3 invocation steps " +
        "does not exceed minimal value of 1500000 WAVES."
    )
  }
}
