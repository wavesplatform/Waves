package com.wavesplatform.it.sync.smartcontract

import com.typesafe.config.Config
import com.wavesplatform.api.http.ApiError.ScriptExecutionError
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.it.NodeConfigs
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.sync._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.transaction.smart.script.ScriptCompiler

class SelfPaymentDappToDappSuite extends BaseTransactionSuite {

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(
        _.preactivatedFeatures(
          (BlockchainFeatures.Ride4DApps.id, 0),
          (BlockchainFeatures.BlockV5.id, 0),
          (BlockchainFeatures.SynchronousCalls.id, 0)
        )
      )
      .withDefault(1)
      .buildNonConflicting()

  private lazy val (caller, callerAddress) = (firstKeyPair, firstAddress)
  private lazy val (dApp1, dAppAddress1)   = (secondKeyPair, secondAddress)
  private lazy val (dApp2, dAppAddress2)   = (thirdKeyPair, thirdAddress)

  private val dAppScript1 = ScriptCompiler(
    s"""
       |{-# STDLIB_VERSION 5 #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |
       |@Callable (i)
       |func foo() = {
       |  strict inv = invoke(this, "bar", [], [AttachedPayment(unit, 100)])
       |  ([], nil)
       |}
       |
       |@Callable (i)
       |func bar() = {
       |  ([], nil)
       |}
       |
         """.stripMargin,
    isAssetScript = false,
    ScriptEstimatorV3.latest
  ).explicitGet()._1.bytes().base64

  private val dAppScript2 = ScriptCompiler(
    s"""
       |{-# STDLIB_VERSION 5 #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |
       |@Callable (i)
       |func foo() = {
       |  strict inv = invoke(i.caller, "foo", [], [AttachedPayment(unit, 100)])
       |  ([], nil)
       |}
       |
       |@Callable (i)
       |func bar() = {
       |  strict inv = invoke(i.caller, "bar", [], [AttachedPayment(unit, 100)])
       |  ([], nil)
       |}
       |
         """.stripMargin,
    isAssetScript = false,
    ScriptEstimatorV3.latest
  ).explicitGet()._1.bytes().base64

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    sender.setScript(dApp1, Some(dAppScript1), waitForTx = true)
    sender.setScript(dApp2, Some(dAppScript2), waitForTx = true)
  }

  test("self payment fails when dapp invokes itself") {
    val callerBalanceBefore = sender.balance(callerAddress).balance
    val dApp1BalanceBefore  = sender.balance(dAppAddress1).balance
    assertApiError(
      sender.invokeScript(caller, dAppAddress1, Some("foo"), fee = 2 * invokeFee, waitForTx = true),
      AssertiveApiError(
        ScriptExecutionError.Id,
        "DApp self-payment is forbidden since V4",
        matchMessage = true
      )
    )
    assertApiError(
      sender.invokeScript(dApp1, dAppAddress2, Some("foo"), fee = 2 * invokeFee, waitForTx = true),
      AssertiveApiError(
        ScriptExecutionError.Id,
        s"The invocation stack contains multiple invocations of the dApp at address $dAppAddress1 with invocations of another dApp between them",
        matchMessage = true
      )
    )
    sender.balance(callerAddress).balance shouldBe callerBalanceBefore
    sender.balance(dAppAddress1).balance shouldBe dApp1BalanceBefore
  }

  test("self payment fails including if invoked dApp calls caller dApp with payment") {
    val callerBalanceBefore = sender.balance(dAppAddress1).balance
    val invFee              = 2 * invokeFee + smartFee
    assertApiError(
      sender.invokeScript(dApp1, dAppAddress2, Some("bar"), fee = invFee, waitForTx = true),
      AssertiveApiError(
        ScriptExecutionError.Id,
        s"The invocation stack contains multiple invocations of the dApp at address $dAppAddress1 with invocations of another dApp between them",
        matchMessage = true
      )
    )
    sender.balance(dAppAddress1).balance shouldBe callerBalanceBefore
  }
}
