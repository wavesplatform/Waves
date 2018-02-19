package com.wavesplatform.state2.reader

import com.wavesplatform.state2.diffs._
import com.wavesplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.{GenesisTransaction, Transaction}

class StateReaderLastTransactionsTest extends PropSpec
  with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  val preconditionsAndPayment: Gen[(Seq[Transaction], TransferTransaction)] = for {
    master <- accountGen
    recipient <- accountGen
    ts <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    transfer1: TransferTransaction <- wavesTransferGeneratorP(ts + 1, master, recipient)
    transfer2: TransferTransaction <- wavesTransferGeneratorP(ts + 2, master, recipient)
    preconditions: Seq[Transaction] = Seq(genesis, transfer1, transfer2)

    transfer3: TransferTransaction <- wavesTransferGeneratorP(ts + 3, master, recipient)
  } yield (preconditions, transfer3)


  property("accountTransactions sort results by 'fresh head' rule") {
    forAll(preconditionsAndPayment) { case ((pre, payment)) =>
      assertDiffAndState(db, Seq(TestBlock.create(pre)), TestBlock.create(Seq(payment))) { (_, newState) =>

        def transactions(count: Int) = newState.accountTransactions(payment.sender, count).map(_._2)

        transactions(1) shouldBe Seq(payment)
        val g = pre.head
        val tx1 = pre(1)
        val tx2 = pre(2)
        transactions(3) shouldBe Seq(payment, tx2, tx1)
        transactions(10) shouldBe Seq(payment, tx2, tx1, g)
        newState.accountTransactionIds(TestBlock.defaultSigner, 10).size shouldBe 0
      }
    }
  }
}
