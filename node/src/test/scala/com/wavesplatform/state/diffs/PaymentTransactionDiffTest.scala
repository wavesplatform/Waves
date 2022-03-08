package com.wavesplatform.state.diffs

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.{GenesisTransaction, PaymentTransaction}
import org.scalacheck.Gen

class PaymentTransactionDiffTest extends PropSpec with WithState {

  val preconditionsAndPayments: Gen[(GenesisTransaction, PaymentTransaction, PaymentTransaction)] = for {
    master    <- accountGen
    recipient <- otherAccountGen(candidate = master)
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
    paymentV2: PaymentTransaction <- paymentGeneratorP(master, recipient.toAddress)
    paymentV3: PaymentTransaction <- paymentGeneratorP(master, recipient.toAddress)
  } yield (genesis, paymentV2, paymentV3)

  val settings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 2)

  property("Diff doesn't break invariant before block version 3") {
    forAll(preconditionsAndPayments) {
      case ((genesis, paymentV2, _)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(paymentV2)), settings) { (blockDiff, newState) =>
          val totalPortfolioDiff: Portfolio = blockDiff.portfolios.values.fold(Portfolio())(_.combine(_).explicitGet())
          totalPortfolioDiff.balance shouldBe 0
          totalPortfolioDiff.effectiveBalance.explicitGet() shouldBe 0
        }
    }
  }

  property("Validation fails with block version 3") {
    forAll(preconditionsAndPayments) {
      case ((genesis, paymentV2, paymentV3)) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(paymentV2))), TestBlock.create(Seq(paymentV3)), settings) {
          blockDiffEi =>
            blockDiffEi should produce(s"Payment transaction is deprecated after h=${settings.blockVersion3AfterHeight}")
        }
    }
  }

}
