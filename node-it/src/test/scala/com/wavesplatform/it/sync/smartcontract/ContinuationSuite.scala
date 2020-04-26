package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
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
  private val activationHeight = 6

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 2, Seq.empty)
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.ContinuationTransaction.id, activationHeight)))
      .buildNonConflicting()

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

  private val script = {
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

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    lazy val setScriptTx = sender.setScript(dApp, Some(script), setScriptFee)

    assertBadRequestAndMessage(
      setScriptTx,
      "State check failed. Reason: Contract function (foo) is too complex: 8175 > 4000"
    )
  }

  test("can set continuation after activation") {
    nodes.waitForHeight(activationHeight)
    sender.setScript(dApp, Some(script), setScriptFee, waitForTx = true).id

    val scriptInfo = sender.addressScriptInfo(dApp)
    scriptInfo.script.isEmpty shouldBe false
    scriptInfo.scriptText.isEmpty shouldBe false
    scriptInfo.script.get.startsWith("base64:") shouldBe true

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
        "with 3 invocation steps " +
        "does not exceed minimal value of 1500000 WAVES."
    )
  }
}
