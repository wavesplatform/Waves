package com.wavesplatform.state.diffs.smart

import com.wavesplatform.TestValues.invokeFee
import com.wavesplatform.db.WithDomain
import com.wavesplatform.db.WithState.AddrWithBalance
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.EmptyDataEntry
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.test.{PropSpec, produce}
import com.wavesplatform.transaction.{Transaction, TransactionType, TxHelpers, TxWithFee}

class SmartAccountFeeTest extends PropSpec with WithDomain {

  private val activationHeight = 4

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
    featureCheckBlocksPeriod = 1,
    blocksForFeatureActivation = 1,
    preActivatedFeatures = Map(
      BlockchainFeatures.SmartAccounts.id    -> 0,
      BlockchainFeatures.SmartAssets.id      -> 0,
      BlockchainFeatures.Ride4DApps.id       -> 0,
      BlockchainFeatures.FeeSponsorship.id   -> 0,
      BlockchainFeatures.DataTransaction.id  -> 0,
      BlockchainFeatures.BlockReward.id      -> 0,
      BlockchainFeatures.BlockV5.id          -> 0,
      BlockchainFeatures.SynchronousCalls.id -> activationHeight
    )
  )

  private val preconditions = {
    val accountWithPaidVerifier  = TxHelpers.signer(0)
    val accountWithSmallVerifier = TxHelpers.signer(1)
    val accountWithEmptyVerifier = TxHelpers.signer(2)

    val transferFee  = FeeUnit * FeeConstants(TransactionType.Transfer)
    val setScriptFee = FeeUnit * FeeConstants(TransactionType.SetScript)

    val balances = AddrWithBalance.enoughBalances(accountWithPaidVerifier, accountWithSmallVerifier, accountWithEmptyVerifier)

    val setScript = Seq(
      TxHelpers.setScript(accountWithPaidVerifier, scriptWithPaidVerifier, fee = setScriptFee),
      TxHelpers.setScript(accountWithSmallVerifier, scriptWithSmallVerifier, fee = setScriptFee),
      TxHelpers.setScript(accountWithEmptyVerifier, scriptWithEmptyVerifier, fee = setScriptFee)
    )

    val invokeFromEnoughPaidVerifier = TxHelpers.invoke(accountWithSmallVerifier.toAddress, invoker = accountWithPaidVerifier, fee = invokeFee(1))
    val invokeFromPaidVerifier       = () => TxHelpers.invoke(accountWithSmallVerifier.toAddress, invoker = accountWithPaidVerifier)
    val invokeFromSmallVerifier      = () => TxHelpers.invoke(accountWithPaidVerifier.toAddress, invoker = accountWithSmallVerifier)
    val invokeFromEmptyVerifier      = () => TxHelpers.invoke(accountWithPaidVerifier.toAddress, invoker = accountWithEmptyVerifier)

    val transferFromEnoughPaidVerifier =
      TxHelpers.transfer(accountWithPaidVerifier, accountWithSmallVerifier.toAddress, 1, fee = transferFee + ScriptExtraFee)
    val transferFromPaidVerifier  = () => TxHelpers.transfer(accountWithPaidVerifier, accountWithSmallVerifier.toAddress, 1, fee = transferFee)
    val transferFromSmallVerifier = () => TxHelpers.transfer(accountWithSmallVerifier, accountWithPaidVerifier.toAddress, 1, fee = transferFee)
    val transferFromEmptyVerifier = () => TxHelpers.transfer(accountWithEmptyVerifier, accountWithSmallVerifier.toAddress, 1, fee = transferFee)

    val dataFromEnoughPaidVerifier = TxHelpers.dataV2(accountWithPaidVerifier, Seq(EmptyDataEntry("key")), fee = transferFee + ScriptExtraFee)
    val dataFromPaidVerifier       = () => TxHelpers.dataV2(accountWithPaidVerifier, Seq(EmptyDataEntry("key")), fee = transferFee)
    val dataFromSmallVerifier      = () => TxHelpers.dataV2(accountWithSmallVerifier, Seq(EmptyDataEntry("key")), fee = transferFee)
    val dataFromEmptyVerifier      = () => TxHelpers.dataV2(accountWithEmptyVerifier, Seq(EmptyDataEntry("key")), fee = transferFee)

    (
      balances,
      setScript,
      List(invokeFromEnoughPaidVerifier, transferFromEnoughPaidVerifier, dataFromEnoughPaidVerifier),
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

  private def appendAndAssertNotEnoughFee(tx: Transaction & TxWithFee, d: Domain) = {
    d.appendBlockE(tx) should produce(
      "TransactionValidationError(cause = GenericError(Transaction sent from smart account. " +
        s"Requires $ScriptExtraFee extra fee. " +
        s"Fee for ${tx.tpe.transactionName} (${tx.fee} in WAVES) " +
        s"does not exceed minimal value of ${FeeConstants(tx.tpe) * FeeUnit + ScriptExtraFee} WAVES.)"
    )
  }

  property(s"small verifier is free after ${BlockchainFeatures.SynchronousCalls} activation") {
    val (balances, preparingTxs, enoughPaidVerifierTxs, notEnoughPaidVerifierTxs, freeVerifierTxs) = preconditions
    withDomain(domainSettingsWithFS(features), balances) { d =>
      d.appendBlock(preparingTxs*)

      (notEnoughPaidVerifierTxs ::: freeVerifierTxs).foreach(tx => appendAndAssertNotEnoughFee(tx(), d))
      d.appendBlock(enoughPaidVerifierTxs: _*)

      d.appendBlock()
      d.blockchain.height shouldBe activationHeight
      d.blockchain.bestLiquidSnapshot.get.scriptsComplexity shouldBe 0

      notEnoughPaidVerifierTxs.foreach(tx => appendAndAssertNotEnoughFee(tx(), d))
      d.appendAndAssertSucceed(freeVerifierTxs.map(_())*)
      d.liquidSnapshot.scriptsComplexity should be > 0L
      d.liquidSnapshot.scriptResults.size shouldBe 2
    }
  }
}
