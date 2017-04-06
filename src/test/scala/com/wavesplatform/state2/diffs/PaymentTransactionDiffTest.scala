package com.wavesplatform.state2.diffs

import cats.Monoid
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{GenesisTransaction, PaymentTransaction, Transaction, TransactionGen}

class PaymentTransactionDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndPayment: Gen[(Seq[Transaction], PaymentTransaction)] = for {
    master <- accountGen
    recipient <- recipientGen(candidate = master)
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    preconditions: Seq[Transaction] = Seq(genesis)
    transfer: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (preconditions, transfer)


  property("Diff doesn't break invariant") {
    forAll(preconditionsAndPayment, accountGen) { case ((pre, payment), miner) =>
      assertDiffAndState(Seq(TestBlock(pre)), TestBlock(Seq(payment), miner)) { (blockDiff, newState) =>
        val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0

        newState.accountTransactionIds(payment.sender).size shouldBe 2 // genesis and payment
      }
    }
  }
}
