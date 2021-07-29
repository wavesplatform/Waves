package com.wavesplatform.state.diffs.smart
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain
import com.wavesplatform.lang.directives.values.V4
import com.wavesplatform.lang.v1.compiler.TestCompiler
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state.EmptyDataEntry
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.state.diffs.FeeValidation.{FeeConstants, FeeUnit}
import com.wavesplatform.state.diffs.ci.ciFee
import com.wavesplatform.test._
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.smart.SetScriptTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction
import com.wavesplatform.transaction.{DataTransaction, GenesisTransaction, Transaction, TransactionType, TxWithFee}

class SmartAccountFeeTest extends PropSpec with WithDomain {

  private val time = new TestTime
  private def ts   = time.getTimestamp()

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

  private val preconditions =
    for {
      accountWithPaidVerifier  <- accountGen
      accountWithSmallVerifier <- accountGen
      accountWithEmptyVerifier <- accountGen
      transferFee  = FeeUnit * FeeConstants(TransactionType.Transfer)
      setScriptFee = FeeUnit * FeeConstants(TransactionType.SetScript)
      invokeFee <- ciFee()
    } yield {
      for {
        genesis    <- GenesisTransaction.create(accountWithPaidVerifier.toAddress, ENOUGH_AMT, ts)
        genesis2   <- GenesisTransaction.create(accountWithSmallVerifier.toAddress, ENOUGH_AMT, ts)
        genesis3   <- GenesisTransaction.create(accountWithEmptyVerifier.toAddress, ENOUGH_AMT, ts)
        setScript  <- SetScriptTransaction.selfSigned(1.toByte, accountWithPaidVerifier, Some(scriptWithPaidVerifier), setScriptFee, ts)
        setScript2 <- SetScriptTransaction.selfSigned(1.toByte, accountWithSmallVerifier, Some(scriptWithSmallVerifier), setScriptFee, ts)
        setScript3 <- SetScriptTransaction.selfSigned(1.toByte, accountWithEmptyVerifier, Some(scriptWithEmptyVerifier), setScriptFee, ts)
        invokeFromPaidVerifier = () =>
          Signed.invokeScript(
              1.toByte,
              accountWithPaidVerifier,
              accountWithSmallVerifier.toAddress,
              None,
              Nil,
              invokeFee,
              Waves,
              ts
            )
        invokeFromSmallVerifier = () =>
          Signed.invokeScript(
              1.toByte,
              accountWithSmallVerifier,
              accountWithPaidVerifier.toAddress,
              None,
              Nil,
              invokeFee,
              Waves,
              ts
            )
        invokeFromEmptyVerifier = () =>
          Signed.invokeScript(
              1.toByte,
              accountWithEmptyVerifier,
              accountWithPaidVerifier.toAddress,
              None,
              Nil,
              invokeFee,
              Waves,
              ts
            )
        transferFromSmallVerifier = () =>
          TransferTransaction
            .selfSigned(
              2.toByte,
              accountWithSmallVerifier,
              accountWithPaidVerifier.toAddress,
              Waves,
              1,
              Waves,
              transferFee,
              ByteStr.empty,
              ts
            )
            .explicitGet()
        transferFromPaidVerifier = () =>
          TransferTransaction
            .selfSigned(
              2.toByte,
              accountWithPaidVerifier,
              accountWithSmallVerifier.toAddress,
              Waves,
              1,
              Waves,
              transferFee,
              ByteStr.empty,
              ts
            )
            .explicitGet()
        transferFromEmptyVerifier = () =>
          TransferTransaction
            .selfSigned(
              2.toByte,
              accountWithEmptyVerifier,
              accountWithSmallVerifier.toAddress,
              Waves,
              1,
              Waves,
              transferFee,
              ByteStr.empty,
              ts
            )
            .explicitGet()
        dataFromSmallVerifier = () =>
          DataTransaction
            .selfSigned(
              2.toByte,
              accountWithSmallVerifier,
              Seq(EmptyDataEntry("key")),
              transferFee,
              ts
            )
            .explicitGet()
        dataFromPaidVerifier = () =>
          DataTransaction
            .selfSigned(
              2.toByte,
              accountWithPaidVerifier,
              Seq(EmptyDataEntry("key")),
              transferFee,
              ts
            )
            .explicitGet()
        dataFromEmptyVerifier = () =>
          DataTransaction
            .selfSigned(
              2.toByte,
              accountWithEmptyVerifier,
              Seq(EmptyDataEntry("key")),
              transferFee,
              ts
            )
            .explicitGet()
      } yield (
        List(genesis, genesis2, genesis3, setScript, setScript2, setScript3),
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
    }.explicitGet()

  private def appendAndAssertNotEnoughFee(tx: Transaction with TxWithFee, d: Domain) = {
    val e = the[RuntimeException] thrownBy d.appendBlock(tx)
    e.getMessage should startWith
    "TransactionValidationError(cause = GenericError(Transaction sent from smart account. " +
      s"Requires $ScriptExtraFee extra fee.. " +
      s"Fee for ${tx.tpe.transactionName} (${tx.fee} in WAVES) " +
      s"does not exceed minimal value of ${FeeConstants(tx.tpe) * FeeUnit + ScriptExtraFee} WAVES.)"
  }

  private def assertNoError(tx: Transaction, d: Domain) =
    d.blockchain.bestLiquidDiff.get.errorMessage(tx.id()) shouldBe None

  property(s"small verifier is free after ${BlockchainFeatures.SynchronousCalls} activation") {
    forAll(preconditions) {
      case (preparingTxs, paidVerifierTxs, freeVerifierTxs) =>
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
}
