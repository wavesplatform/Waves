package com.wavesplatform.state.diffs

import com.wavesplatform.consensus.GeneratingBalanceProvider
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state.Height
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.WavesSettingsOps
import com.wavesplatform.transaction.{CommitToGenerationTransaction, TxHelpers}
import com.wavesplatform.utils.Numbers

class SpendingTableTest extends FreeSpec with WithState {
  private enum Spending {
    case Leasing
    case Deposit
    case Transfer
  }

  "spending more than have" - {
    def test(blockVersion3AfterHeight: Int)(hasLeasing: Boolean, hasDeposit: Boolean, spending: Spending, expectedError: String): Unit =
      s"hasLeasing=$hasLeasing, hasDeposit=$hasDeposit, spending=$spending" in {
        val settings = DomainPresets.DeterministicFinality
          .addFeatures(BlockchainFeatures.SmallerMinimalGeneratingBalance)
          .blockchainSettings
          .functionalitySettings
          .copy(
            blockVersion3AfterHeight = blockVersion3AfterHeight,
            generationPeriodLength = 2
          )

        val spendingAmount = CommitToGenerationTransaction.DepositInWavelets + // To fit both leasing and deposit cases
          Numbers.when(hasDeposit || spending == Spending.Deposit)(GeneratingBalanceProvider.MinimalEffectiveBalanceForGenerator2)

        val miner     = TxHelpers.signer(0)
        val minerAddr = miner.toAddress

        val spender     = TxHelpers.signer(1)
        val spenderAddr = spender.toAddress

        val txFee       = 1.waves
        val initLeasing = Numbers.when(hasLeasing)(11.waves)
        val initDeposit = Numbers.when(hasDeposit)(CommitToGenerationTransaction.DepositInWavelets)
        val initBalance = (spendingAmount - 1) + txFee + // Less, than required
          Numbers.when(initLeasing > 0)(initLeasing + txFee) + Numbers.when(initDeposit > 0)(initDeposit + txFee)

        val initLeasingTx = Option.when(hasLeasing)(TxHelpers.lease(sender = spender, minerAddr, amount = initLeasing, fee = txFee))
        val initDepositTx = Option.when(hasDeposit)(TxHelpers.commitToGeneration(sender = spender, generationPeriodStart = Height(3), fee = txFee))
        val spendingTx = spending match {
          case Spending.Leasing  => TxHelpers.lease(sender = spender, minerAddr, amount = spendingAmount, fee = txFee)
          case Spending.Deposit  => TxHelpers.commitToGeneration(sender = spender, generationPeriodStart = Height(5), fee = txFee)
          case Spending.Transfer => TxHelpers.transfer(from = spender, to = minerAddr, amount = spendingAmount, fee = txFee)
        }

        assertDiffEiTraced(
          Seq(
            TestBlock.create(
              Seq(
                TxHelpers.genesis(minerAddr, amount = 10_000.waves),
                TxHelpers.genesis(spenderAddr, amount = initBalance)
              )
            ),
            TestBlock.create(miner, Seq(initLeasingTx, initDepositTx).flatten)
          ),
          TestBlock.create(miner, Seq(spendingTx)),
          settings
        ) { snapshotEi =>
          snapshotEi.resultE should produce(expectedError)
        }
      }

    "blockVersion3AfterHeight in the future" - Table(
      ("hasLeasing", "hasDeposit", "spending", "expectedError"),
      (false, false, Spending.Leasing, "Cannot lease more than own: Balance: 10099999999, already leased: 0"), // DiffsCommon
      // CommitToGenerationTransactionDiff
      (false, false, Spending.Deposit, "Generating balance 99999999999 is less than 100000000000 required for block generation"),
      // CommonValidation
      (false, false, Spending.Transfer, "Transaction application leads to negative waves balance to (at least) temporary negative state"),
      (false, true, Spending.Leasing, "trying to spend either a deposit or leased money"), // BalanceDiffValidation
      // CommitToGenerationTransactionDiff
      (false, true, Spending.Deposit, "Generating balance 99999999999 is less than 100000000000 required"),
      (false, true, Spending.Transfer, "trying to spend a deposit"), // BalanceDiffValidation
      // (true, false, Spending.Leasing, success), // Covered by BalanceDiffValidationTest
      // CommitToGenerationTransactionDiff
      (true, false, Spending.Deposit, "Generating balance 99999999999 is less than 100000000000 required for block generation"),
      // (true, false, Spending.Transfer, success), // Covered by BalanceDiffValidationTest
      (true, true, Spending.Leasing, "trying to spend either a deposit or leased money"), // BalanceDiffValidation
      // CommitToGenerationTransactionDiff
      (true, true, Spending.Deposit, "Generating balance 99999999999 is less than 100000000000 required for block generation"),
      (true, true, Spending.Transfer, "trying to spend either a deposit or leased money") // BalanceDiffValidation
    ).foreach(test(blockVersion3AfterHeight = 1000))

    "blockVersion3AfterHeight from 0" - Table(
      ("hasLeasing", "hasDeposit", "spending", "expectedError"),
      (false, false, Spending.Leasing, "Cannot lease more than own: Balance: 10099999999, already leased: 0"), // DiffsCommon
      // CommitToGenerationTransactionDiff
      (false, false, Spending.Deposit, "Generating balance 99999999999 is less than 100000000000 required for block generation"),
      // CommonValidation
      (false, false, Spending.Transfer, "Transaction application leads to negative waves balance to (at least) temporary negative state"),
      (false, true, Spending.Leasing, "negative effective balance"), // BalanceDiffValidation
      // CommitToGenerationTransactionDiff
      (false, true, Spending.Deposit, "Generating balance 99999999999 is less than 100000000000 required"),
      (false, true, Spending.Transfer, "trying to spend a deposit"),                                                   // BalanceDiffValidation
      (true, false, Spending.Leasing, "Cannot lease more than own: Balance: 11199999999, already leased: 1100000000"), // DiffsCommon
      // CommitToGenerationTransactionDiff
      (true, false, Spending.Deposit, "Generating balance 99999999999 is less than 100000000000 required for block generation"),
      (true, false, Spending.Transfer, "negative effective balance"), // BalanceDiffValidation
      (true, true, Spending.Leasing, "negative effective balance"),   // BalanceDiffValidation
      // CommitToGenerationTransactionDiff
      (true, true, Spending.Deposit, "Generating balance 99999999999 is less than 100000000000 required for block generation"),
      (true, true, Spending.Transfer, "negative effective balance") // BalanceDiffValidation
    ).foreach(test(blockVersion3AfterHeight = 0))
  }
}
