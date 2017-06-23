package com.wavesplatform.state2.reader

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2.diffs._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{GenesisTransaction, PaymentTransaction, Transaction}

class StateReaderLastTransactionsTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndPayment: Gen[(Seq[Transaction], PaymentTransaction)] = for {
    master <- accountGen
    recipient <- accountGen
    ts <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    transfer1: PaymentTransaction <- paymentGeneratorP(ts + 1, master, recipient)
    transfer2: PaymentTransaction <- paymentGeneratorP(ts + 2, master, recipient)
    preconditions: Seq[Transaction] = Seq(genesis, transfer1, transfer2)

    transfer3: PaymentTransaction <- paymentGeneratorP(ts + 3, master, recipient)
  } yield (preconditions, transfer3)


  property("accountTransactions sort results by 'fresh head' rule") {
    forAll(preconditionsAndPayment) { case ((pre, payment)) =>
      assertDiffAndState(Seq(TestBlock.create(pre)), TestBlock.create(Seq(payment))) { (blockDiff, newState) =>

        newState.accountTransactions(payment.sender, 1) shouldBe Seq(payment)
        val g = pre.head
        val tx1 = pre(1)
        val tx2 = pre(2)
        newState.accountTransactions(payment.sender, 3) shouldBe Seq(payment, tx2, tx1)
        newState.accountTransactions(payment.sender, 10) shouldBe Seq(payment, tx2, tx1, g)
      }
    }
  }
}
