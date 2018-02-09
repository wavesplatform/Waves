package com.wavesplatform.state2.diffs

import cats.Monoid
import com.wavesplatform.state2._
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.{GenesisTransaction, PaymentTransaction}

class PaymentTransactionDiffTest extends PropSpec
  with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  val preconditionsAndPayments: Gen[(GenesisTransaction, PaymentTransaction, PaymentTransaction)] = for {
    master <- accountGen
    recipient <- otherAccountGen(candidate = master)
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    paymentV2: PaymentTransaction <- paymentGeneratorP(master, recipient)
    paymentV3: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (genesis, paymentV2, paymentV3)

  val settings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 2)

  property("Diff doesn't break invariant before block version 3") {
    forAll(preconditionsAndPayments) { case ((genesis, paymentV2, paymentV3)) =>
      assertDiffAndState(db, Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(paymentV2)), settings) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        newState.accountTransactionIds(paymentV2.sender, 2).size shouldBe 2 // genesis and payment
      }
    }
  }

  property("Validation fails with block version 3") {
    forAll(preconditionsAndPayments) { case ((genesis, paymentV2, paymentV3)) =>
      assertDiffEi(db, Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(paymentV2))), TestBlock.create(Seq(paymentV3)), settings) { blockDiffEi =>
        blockDiffEi should produce(s"Payment transaction is deprecated after h=${settings.blockVersion3AfterHeight}")
      }
    }
  }
}
