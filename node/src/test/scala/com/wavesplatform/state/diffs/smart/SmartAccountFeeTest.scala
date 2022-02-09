package com.wavesplatform.state.diffs.smart

import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.{Constants, TestFunctionalitySettings}
import com.wavesplatform.state.EmptyDataEntry
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.transaction.{Transaction, TxHelpers, TxWithFee}
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction

class SmartAccountFeeTest extends PropSpec with WithDomain {

  private val activationHeight = 3

  private val scriptWithEmptyVerifier = TestCompiler(V4).compileContract("""
    | {-# STDLIB_VERSION 4       #-}
    | {-# CONTENT_TYPE   DAPP    #-}
    | {-# SCRIPT_TYPE    ACCOUNT #-}
    |
    |""".stripMargin)

  private val scriptWithSmallVerifier = TestCompiler(V4).compileContract("""
    | {-# STDLIB_VERSION 4       #-}
    | {-# CONTENT_TYPE   DAPP    #-}
    | {-# SCRIPT_TYPE    ACCOUNT #-}
    |
    | @Verifier(tx)
    | func verify() =
    |   sigVerify_16Kb(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey) &&
    |   sigVerify_16Kb(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey) &&
    |   sigVerify_8Kb(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
    |
    | @Callable(i)
    | func default() = []
    |""".stripMargin)

  private val scriptWithPaidVerifier = TestCompiler(V4).compileContract("""
    | {-# STDLIB_VERSION 4       #-}
    | {-# CONTENT_TYPE   DAPP    #-}
    | {-# SCRIPT_TYPE    ACCOUNT #-}
    |
    | @Verifier(tx)
    | func verify() =
    |   sigVerify_16Kb(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey) &&
    |   sigVerify_16Kb(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey) &&
    |   sigVerify_16Kb(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey)
    |
    | @Callable(i)
    | func default() = []
    |""".stripMargin)

  private val features = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id    -> 0,
      BlockchainFeatures.SmartAssets.id      -> 0,
      BlockchainFeatures.Ride4DApps.id       -> 0,
      BlockchainFeatures.FeeSponsorship.id   -> 0,
      BlockchainFeatures.DataTransaction.id  -> 0,
      BlockchainFeatures.BlockReward.id      -> 0,
      BlockchainFeatures.BlockV5.id          -> 0,
      BlockchainFeatures.SynchronousCalls.id -> activationHeight
    ),
    featureCheckBlocksPeriod = 1,
    blocksForFeatureActivation = 1
  )

  private val preconditions = {
    val accountWithPaidVerifier = TxHelpers.signer(0)
    val accountWithSmallVerifier = TxHelpers.signer(1)
    val accountWithEmptyVerifier = TxHelpers.signer(2)

    val transferFee  = FeeUnit * FeeConstants(TransferTransaction.typeId)
    val setScriptFee = FeeUnit * FeeConstants(SetScriptTransaction.typeId)

    val genesis = Seq(
      TxHelpers.genesis(accountWithPaidVerifier.toAddress),
      TxHelpers.genesis(accountWithSmallVerifier.toAddress),
      TxHelpers.genesis(accountWithEmptyVerifier.toAddress)
    )
    val setScript = Seq(
      TxHelpers.setScript(accountWithPaidVerifier, scriptWithPaidVerifier, fee = setScriptFee),
      TxHelpers.setScript(accountWithSmallVerifier, scriptWithSmallVerifier, fee = setScriptFee),
      TxHelpers.setScript(accountWithEmptyVerifier, scriptWithEmptyVerifier, fee = setScriptFee)
    )

    val invokeFromPaidVerifier = () => TxHelpers.invoke(accountWithSmallVerifier.toAddress, invoker = accountWithPaidVerifier)
    val invokeFromSmallVerifier = () => TxHelpers.invoke(accountWithPaidVerifier.toAddress, invoker = accountWithSmallVerifier)
    val invokeFromEmptyVerifier = () => TxHelpers.invoke(accountWithPaidVerifier.toAddress, invoker = accountWithEmptyVerifier)

    val transferFromSmallVerifier = () => TxHelpers.transfer(accountWithSmallVerifier, accountWithPaidVerifier.toAddress, 1, fee = transferFee)
    val transferFromPaidVerifier = () => TxHelpers.transfer(accountWithPaidVerifier, accountWithSmallVerifier.toAddress, 1, fee = transferFee)
    val transferFromEmptyVerifier = () => TxHelpers.transfer(accountWithEmptyVerifier, accountWithSmallVerifier.toAddress, 1, fee = transferFee)

    val dataFromSmallVerifier = () => TxHelpers.dataV2(accountWithSmallVerifier, Seq(EmptyDataEntry("key")), fee = transferFee)
    val dataFromPaidVerifier = () => TxHelpers.dataV2(accountWithPaidVerifier, Seq(EmptyDataEntry("key")), fee = transferFee)
    val dataFromEmptyVerifier = () => TxHelpers.dataV2(accountWithEmptyVerifier, Seq(EmptyDataEntry("key")), fee = transferFee)

    (
      genesis ++ setScript,
      List(invokeFromPaidVerifier, transferFromPaidVerifier, dataFromPaidVerifier),
      List(
        invokeFromSmallVerifier,
        transferFromSmallVerifier,
        dataFromSmallVerifier,
        invokeFromEmptyVerifier,
        transferFromEmptyVerifier,
        dataFromEmptyVerifier
      )
    )
  }

  private def appendAndAssertNotEnoughFee(tx: Transaction with TxWithFee, d: Domain) = {
    val e = the[RuntimeException] thrownBy d.appendBlock(tx)
    e.getMessage should startWith
    "TransactionValidationError(cause = GenericError(Transaction sent from smart account. " +
      s"Requires $ScriptExtraFee extra fee.. " +
      s"Fee for ${Constants.TransactionNames(tx.typeId)} (${tx.fee} in WAVES) " +
      s"does not exceed minimal value of ${FeeConstants(tx.typeId) * FeeUnit + ScriptExtraFee} WAVES.)"
  }

  private def assertNoError(tx: Transaction, d: Domain) =
    d.blockchain.bestLiquidDiff.get.errorMessage(tx.id()) shouldBe None

  property(s"small verifier is free after ${BlockchainFeatures.SynchronousCalls} activation") {
    val (preparingTxs, paidVerifierTxs, freeVerifierTxs) = preconditions
    withDomain(domainSettingsWithFS(features)) { d =>
      d.appendBlock(preparingTxs: _*)

      (paidVerifierTxs ::: freeVerifierTxs).foreach(tx => appendAndAssertNotEnoughFee(tx(), d))

      d.appendBlock()
      d.appendBlock()
      d.blockchain.height shouldBe activationHeight
      d.blockchain.bestLiquidDiff.get.scriptsRun shouldBe 0

      paidVerifierTxs.foreach(tx => appendAndAssertNotEnoughFee(tx(), d))
      d.appendBlock(freeVerifierTxs.map(_()): _*)
      freeVerifierTxs.foreach(tx => assertNoError(tx(), d))
      d.blockchain.bestLiquidDiff.get.scriptsRun shouldBe freeVerifierTxs.size
    }
  }
}
