package com.wavesplatform.state2.diffs

import cats.Monoid
import com.wavesplatform.state2._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{GenesisTransaction, PaymentTransaction}

class PaymentTransactionDiffTest extends PropSpec
  with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val preconditionsAndPayment: Gen[(GenesisTransaction, PaymentTransaction)] = for {
    master <- accountGen
    recipient <- otherAccountGen(candidate = master)
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    transfer: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (genesis, transfer)


  property("Diff doesn't break invariant") {
    forAll(preconditionsAndPayment) { case ((genesis, payment)) =>
      assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(payment))) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        newState.accountTransactionIds(payment.sender, 2).size shouldBe 2 // genesis and payment
      }
    }
  }
}
