package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.{GenesisTransaction, PaymentTransaction, TransactionGen}

class LeaseTransactionsDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsLeaseLeaseCancel: Gen[(GenesisTransaction, LeaseTransaction, LeaseCancelTransaction)] = for {
    master <- accountGen
    recipient <- accountGen suchThat (_ != master)
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    (lease, unlease) <- leaseAndCancelGeneratorP(master, recipient, master)
  } yield (genesis, lease, unlease)

  property("can lease/cancel lease preserving waves invariant") {
    forAll(preconditionsLeaseLeaseCancel, accountGen) { case ((genesis, lease, leaseCancel), miner: PrivateKeyAccount) =>
      assertDiffAndState(Seq(TestBlock(Seq(genesis))), TestBlock(Seq(lease), miner)) { case (totalDiff, newState) =>
        val totalPortfolioDiff = Monoid.combineAll(totalDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)

        val snapshot = EffectiveBalanceSnapshot(lease.recipient.asInstanceOf[Account], 2, 0, lease.amount)
        totalDiff.effectiveBalanceSnapshots should contain(snapshot)
      }

      assertDiffAndState(Seq(TestBlock(Seq(genesis, lease))), TestBlock(Seq(leaseCancel), miner)) { case (totalDiff, newState) =>
        val totalPortfolioDiff = Monoid.combineAll(totalDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)

        val snapshot = EffectiveBalanceSnapshot(lease.recipient.asInstanceOf[Account], 2, lease.amount, 0)
        totalDiff.effectiveBalanceSnapshots should contain(snapshot)
      }
    }
  }


  property("cannot cancel lease twice") {
    val setup: Gen[(GenesisTransaction, PaymentTransaction, LeaseTransaction, LeaseCancelTransaction, LeaseCancelTransaction)] = for {
      master <- accountGen
      recpient <- accountGen suchThat (_ != master)
      ts <- positiveLongGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (lease, unlease) <- leaseAndCancelGeneratorP(master, recpient, master)
      fee2 <- smallFeeGen
      unlease2 = LeaseCancelTransaction.create(master, lease.id, fee2, ts + 1).right.get
      // ensure recipient has enough effective balance
      payment <- paymentGeneratorP(ts, master, recpient) suchThat (_.amount > lease.amount)
    } yield (genesis, payment, lease, unlease, unlease2)

    forAll(setup) { case ((genesis, payment, lease, leaseCancel, leaseCancel2)) =>
      assertDiffEi(Seq(TestBlock(Seq(genesis, payment, lease, leaseCancel))), TestBlock(Seq(leaseCancel2))) { totalDiffEi =>
        totalDiffEi should produce("Cannot cancel cancelled lease")
      }
    }
  }

  property("cannot lease more than actual balance(cannot lease forward)") {
    val setup: Gen[(GenesisTransaction, LeaseTransaction, LeaseTransaction)] = for {
      master <- accountGen
      recipient <- accountGen suchThat (_ != master)
      forward <- accountGen suchThat (_ != recipient)
      ts <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (lease, _) <- leaseAndCancelGeneratorP(master, recipient, master)
      (leaseForward, _) <- leaseAndCancelGeneratorP(recipient, forward, recipient)
    } yield (genesis, lease, leaseForward)

    forAll(setup) { case ((genesis, lease, leaseForward)) =>
      assertDiffEi(Seq(TestBlock(Seq(genesis, lease))), TestBlock(Seq(leaseForward))) { totalDiffEi =>
        totalDiffEi should produce("Cannot lease more than own")
      }
    }
  }

  property("cannot cancel lease of another sender") {
    val setup: Gen[(GenesisTransaction, GenesisTransaction, LeaseTransaction, LeaseCancelTransaction)] = for {
      master <- accountGen
      recpient <- accountGen suchThat (_ != master)
      other <- accountGen suchThat (_ != master)
      ts <- positiveLongGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      genesis2: GenesisTransaction = GenesisTransaction.create(other, ENOUGH_AMT, ts).right.get
      (lease, _) <- leaseAndCancelGeneratorP(master, recpient, master)
      fee2 <- smallFeeGen
      unleaseOther = LeaseCancelTransaction.create(other, lease.id, fee2, ts + 1).right.get
    } yield (genesis, genesis2, lease, unleaseOther)

    forAll(setup) { case ((genesis, genesis2, lease, unleaseOther)) =>
      assertDiffEi(Seq(TestBlock(Seq(genesis, genesis2, lease))), TestBlock(Seq(unleaseOther))) { totalDiffEi =>
        totalDiffEi should produce("LeaseTransaction was leased by other sender")
      }
    }
  }

}
