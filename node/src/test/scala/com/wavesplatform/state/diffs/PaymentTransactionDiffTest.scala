package com.wavesplatform.state.diffs

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings}
import com.wavesplatform.state.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.{GenesisTransaction, PaymentTransaction, TxHelpers}

class PaymentTransactionDiffTest extends PropSpec with WithState {

  val preconditionsAndPayments: Seq[(GenesisTransaction, PaymentTransaction, PaymentTransaction)] = {
    val master = TxHelpers.signer(1)
    Seq(master, TxHelpers.signer(2)).map { recipient =>
      val genesis   = TxHelpers.genesis(master.toAddress)
      val paymentV2 = TxHelpers.payment(master, recipient.toAddress)
      val paymentV3 = TxHelpers.payment(master, recipient.toAddress)

      (genesis, paymentV2, paymentV3)
    }
  }

  val settings: FunctionalitySettings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 2)

  property("Diff doesn't break invariant before block version 3") {
    preconditionsAndPayments.foreach { case ((genesis, paymentV2, _)) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(paymentV2)), settings) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = blockDiff.portfolios.values.fold(Portfolio())(_.combine(_).explicitGet())
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance(false).explicitGet() shouldBe 0
      }
    }
  }

  property("Validation fails with block version 3") {
    preconditionsAndPayments.foreach { case ((genesis, paymentV2, paymentV3)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(paymentV2))), TestBlock.create(Seq(paymentV3)), settings) { blockDiffEi =>
        blockDiffEi should produce(s"Payment transaction is deprecated after h=${settings.blockVersion3AfterHeight}")
      }
    }
  }
}
