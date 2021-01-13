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
          (BlockchainFeatures.Ride4DApps, 0),
          (BlockchainFeatures.BlockV5, 0),
          (BlockchainFeatures.SynchronousCalls, 0)
        )
      )
      .withDefault(1)
      .buildNonConflicting()

  private lazy val (caller, callerAddress) = (firstKeyPair, firstAddress)
  private lazy val (dApp1, dAppAddress1) = (secondKeyPair, secondAddress)
  private lazy val (dApp2, dAppAddress2) = (thirdKeyPair, thirdAddress)

  private val dAppScript1 = ScriptCompiler(
    s"""
       |{-# STDLIB_VERSION 5 #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |
       |@Callable (i)
       |func foo() = {
       |  strict inv = Invoke(this, "bar", [], [AttachedPayment(unit, 100)])
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
    ScriptEstimatorV3
  ).explicitGet()._1.bytes().base64
  private val dAppScript2 = ScriptCompiler(
    s"""
       |{-# STDLIB_VERSION 5 #-}
       |{-# CONTENT_TYPE DAPP #-}
       |{-# SCRIPT_TYPE ACCOUNT #-}
       |
       |@Callable (i)
       |func foo() = {
       |  strict inv = Invoke(i.caller, "foo", [], [AttachedPayment(unit, 100)])
       |  ([], nil)
       |}
       |
       |@Callable (i)
       |func bar() = {
       |  strict inv = Invoke(i.caller, "bar", [], [AttachedPayment(unit, 100)])
       |  ([], nil)
       |}
       |
         """.stripMargin,
    isAssetScript = false,
    ScriptEstimatorV3
  ).explicitGet()._1.bytes().base64

  protected override def beforeAll(): Unit = {
    super.beforeAll()
    miner.setScript(dApp1, Some(dAppScript1), waitForTx = true)
    miner.setScript(dApp2, Some(dAppScript2), waitForTx = true)
  }

  test("self payment fails when dapp invokes itself") {
    val callerBalanceBefore = miner.balanceDetails(callerAddress).regular
    val dApp1BalanceBefore = miner.balanceDetails(dAppAddress1).regular
    assertApiError(
      miner.invokeScript(caller, dAppAddress1, Some("foo"), fee = 2 * invokeFee, waitForTx = true),
      AssertiveApiError(
        ScriptExecutionError.Id,
        "Error while executing account-script: FailedTransactionError(code = 1, error = DApp self-payment is forbidden since V4, log =)"
      )
    )
    assertApiError(
      miner.invokeScript(dApp1, dAppAddress2, Some("foo"), fee = 2 * invokeFee, waitForTx = true),
      AssertiveApiError(
        ScriptExecutionError.Id,
        s"Error while executing account-script: " +
          s"GenericError(Complex dApp recursion is prohibited, but dApp at address $dAppAddress1 was called twice)"
      )
    )
    miner.balanceDetails(callerAddress).regular shouldBe callerBalanceBefore
    miner.balanceDetails(dAppAddress1).regular shouldBe dApp1BalanceBefore
  }

  test("self payment fails including if invoked dApp calls caller dApp with payment") {
    val callerBalanceBefore = miner.balanceDetails(dAppAddress1).regular
    val invFee = 2 * invokeFee + smartFee
    assertApiError(
      miner.invokeScript(dApp1, dAppAddress2, Some("bar"), fee = invFee, waitForTx = true),
      AssertiveApiError(
        ScriptExecutionError.Id,
        s"Error while executing account-script: " +
          s"GenericError(Complex dApp recursion is prohibited, but dApp at address $dAppAddress1 was called twice)"
      )
    )
    miner.balanceDetails(dAppAddress1).regular shouldBe callerBalanceBefore
  }
}
