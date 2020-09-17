package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.account.KeyPair
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.NodeConfigs.Default
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.{DebugStateChanges, TransactionInfo}
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxVersion
import com.wavesplatform.transaction.smart.ContinuationTransaction
import com.wavesplatform.transaction.smart.InvokeScriptTransaction.Payment
import com.wavesplatform.transaction.smart.script.ScriptCompiler
import monix.eval.Coeval
import org.scalatest.{Assertion, CancelAfterFailure, OptionValues}

class ContinuationSuite extends BaseTransactionSuite with CancelAfterFailure with OptionValues {
  private val activationHeight = 6

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs
      .Builder(Default, 2, Seq.empty)
      .overrideBase(_.preactivatedFeatures((BlockchainFeatures.ContinuationTransaction.id, activationHeight)))
      .overrideBase(_.raw("waves.blockchain.use-evaluator-v2 = true"))
      .buildNonConflicting()

  private var dApp: KeyPair   = _
  private var caller: KeyPair = _

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
         |  let a =
         |    height == $activationHeight                                                  &&
         |    getInteger(Address(base58''), "key") == unit                                 &&
         |    !(${List.fill(150)("sigVerify(base64'',base64'',base64'')").mkString("||")}) &&
         |    height == $activationHeight
         |  [BooleanEntry("a", a), BinaryEntry("sender", inv.caller.bytes)]
         | }
         |
       """.stripMargin
    ScriptCompiler.compile(scriptText, dummyEstimator).explicitGet()._1.bytes().base64
  }

  protected override def beforeAll(): Unit = {
    super.beforeAll()

    dApp = firstKeyPair
    caller = secondKeyPair

    lazy val setScriptTx = sender.setScript(dApp, Some(script), setScriptFee)

    assertBadRequestAndMessage(
      setScriptTx,
      "State check failed. Reason: Contract function (foo) is too complex: 30638 > 4000"
    )
  }

  test("can set continuation after activation") {
    nodes.waitForHeight(activationHeight)
    sender.setScript(dApp, Some(script), setScriptFee, waitForTx = true).id

    val scriptInfo = sender.addressScriptInfo(dApp.toAddress.toString)
    scriptInfo.script.isEmpty shouldBe false
    scriptInfo.scriptText.isEmpty shouldBe false
    scriptInfo.script.get.startsWith("base64:") shouldBe true
  }

  test("successful continuation") {
    val invoke = sender.invokeScript(
      caller,
      dApp.toAddress.toString,
      func = Some("foo"),
      args = Nil,
      payment = Seq(Payment(1.waves, Waves)),
      fee = 1.waves,
      version = TxVersion.V2,
      waitForTx = true
    )
    waitForContinuation(invoke._1.id)
    checkContinuationChain(invoke._1.id, sender.height)
    nodes.foreach { node =>
      node.getDataByKey(dApp.toAddress.toString, "a") shouldBe BooleanDataEntry("a", true)
      node.getDataByKey(dApp.toAddress.toString, "sender") shouldBe BinaryDataEntry("sender", ByteStr(Base58.decode(caller.toAddress.toString)))
    }
  }

  private def waitForContinuation(invokeId: String): Boolean = {
    nodes.waitFor(
      s"chain of continuation for InvokeScript Transaction with id = $invokeId is completed"
    )(
      _.blockAt(sender.height - 1)
        .transactions
        .exists { tx =>
          tx._type == ContinuationTransaction.typeId &&
          tx.applicationStatus.contains("succeeded") &&
          tx.invokeScriptTransactionId.contains(invokeId)
        }
    )(
      _.forall(identity)
    )
  }

  private def checkContinuationChain(invokeId: String, completionHeight: Int): Assertion = {
    val invokeInfo = sender.transactionInfo[DebugStateChanges](invokeId)
    invokeInfo.applicationStatus.value shouldBe "script_execution_in_progress"

    val continuations =
      (invokeInfo.height to completionHeight)
        .map(sender.blockAt(_))
        .flatMap(_.transactions)
        .filter(tx => tx._type == ContinuationTransaction.typeId && tx.invokeScriptTransactionId.contains(invokeId))

    continuations.dropRight(1).foreach(_.applicationStatus.value shouldBe "script_execution_in_progress")
    continuations.last.applicationStatus.value shouldBe "succeeded"
    continuations.map(_.nonce.value) shouldBe continuations.indices
    invokeInfo.timestamp +: continuations.map(_.timestamp) shouldBe sorted
  }

  test("insufficient fee") {
    lazy val invokeScriptTx = sender.invokeScript(
      caller,
      dApp.toAddress.toString,
      func = Some("foo"),
      args = Nil,
      payment = Seq(Payment(1.waves, Waves)),
      fee = invokeFee,
      version = TxVersion.V2
    )

    assertBadRequestAndMessage(
      invokeScriptTx,
      "Fee in WAVES for InvokeScriptTransaction (900000 in WAVES) " +
        "with 8 invocation steps " +
        "does not exceed minimal value of 4000000 WAVES."
    )
  }
}
