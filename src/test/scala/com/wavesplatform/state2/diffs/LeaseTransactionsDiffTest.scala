package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.{GenesisTransaction, PaymentTransaction}

class LeaseTransactionsDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  private val allowMultipleLeaseCancelTransactionUntilTimestamp = Long.MaxValue / 2
  private val settings = TestFunctionalitySettings.Enabled.copy(
    allowMultipleLeaseCancelTransactionUntilTimestamp = allowMultipleLeaseCancelTransactionUntilTimestamp)


  def total(l: LeaseInfo): Long = l.leaseIn - l.leaseOut

  property("can lease/cancel lease preserving waves invariant") {

    val sunnyDayLeaseLeaseCancel: Gen[(GenesisTransaction, LeaseTransaction, LeaseCancelTransaction)] = for {
      master <- accountGen
      recipient <- accountGen suchThat (_ != master)
      ts <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (lease, unlease) <- leaseAndCancelGeneratorP(master, recipient, master)
    } yield (genesis, lease, unlease)

    forAll(sunnyDayLeaseLeaseCancel, accountGen) { case ((genesis, lease, leaseCancel), miner: PrivateKeyAccount) =>
      assertDiffAndState(Seq(TestBlock(Seq(genesis))), TestBlock(Seq(lease), miner)) { case (totalDiff, newState) =>
        val totalPortfolioDiff = Monoid.combineAll(totalDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        total(totalPortfolioDiff.leaseInfo) shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)

        totalDiff.snapshots(lease.recipient.asInstanceOf[Account]) shouldBe Map(2 -> Snapshot(0, 0, lease.amount))
      }

      assertDiffAndState(Seq(TestBlock(Seq(genesis, lease))), TestBlock(Seq(leaseCancel), miner)) { case (totalDiff, newState) =>
        val totalPortfolioDiff = Monoid.combineAll(totalDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        total(totalPortfolioDiff.leaseInfo) shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)

        totalDiff.snapshots(lease.recipient.asInstanceOf[Account]) shouldBe Map(2 -> Snapshot(1, 0, 0))

        newState.accountPortfolio(lease.sender).leaseInfo shouldBe LeaseInfo.empty
        newState.accountPortfolio(lease.recipient.asInstanceOf[Account]).leaseInfo shouldBe LeaseInfo.empty
      }
    }
  }

  val cancelLeaseTwice: Gen[(GenesisTransaction, PaymentTransaction, LeaseTransaction, LeaseCancelTransaction, LeaseCancelTransaction)] = for {
    master <- accountGen
    recpient <- accountGen suchThat (_ != master)
    ts <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    (lease, unlease) <- leaseAndCancelGeneratorP(master, recpient, master)
    fee2 <- smallFeeGen
    unlease2 = LeaseCancelTransaction.create(master, lease.id, fee2, ts + 1).right.get
    // ensure recipient has enough effective balance
    payment <- paymentGeneratorP(master, recpient) suchThat (_.amount > lease.amount)
  } yield (genesis, payment, lease, unlease, unlease2)

  property("cannot cancel lease twice after allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseTwice, timestampGen retryUntil (_ > allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, payment, lease, leaseCancel, leaseCancel2), blockTime) =>
        assertDiffEi(Seq(TestBlock(Seq(genesis, payment, lease, leaseCancel))), TestBlock.create(blockTime, Seq(leaseCancel2)), settings) { totalDiffEi =>
          totalDiffEi should produce("Cannot cancel already cancelled lease")
        }
    }
  }

  property("can cancel lease twice before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseTwice, timestampGen retryUntil (_ < allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, payment, lease, leaseCancel, leaseCancel2), blockTime) =>
        assertDiffEi(Seq(TestBlock(Seq(genesis, payment, lease, leaseCancel))), TestBlock.create(blockTime, Seq(leaseCancel2)), settings) { totalDiffEi =>
          totalDiffEi shouldBe 'right
        }
    }
  }

  property("cannot lease more than actual balance(cannot lease forward)") {
    val setup: Gen[(GenesisTransaction, LeaseTransaction, LeaseTransaction)] = for {
      master <- accountGen
      recipient <- accountGen suchThat (_ != master)
      forward <- accountGen suchThat (!Set(master, recipient).contains(_))
      ts <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      (lease, _) <- leaseAndCancelGeneratorP(master, recipient, master)
      (leaseForward, _) <- leaseAndCancelGeneratorP(recipient, forward, recipient)
    } yield (genesis, lease, leaseForward)

    forAll(setup) { case ((genesis, lease, leaseForward)) =>
      assertDiffEi(Seq(TestBlock(Seq(genesis, lease))), TestBlock(Seq(leaseForward)), settings) { totalDiffEi =>
        totalDiffEi should produce("Cannot lease more than own")
      }
    }
  }

  def cancelLeaseOfAnotherSender(unleaseByRecipient: Boolean): Gen[(GenesisTransaction, GenesisTransaction, LeaseTransaction, LeaseCancelTransaction)] = for {
    master <- accountGen
    recipient <- accountGen suchThat (_ != master)
    other <- accountGen suchThat (_ != recipient)
    unleaser = if (unleaseByRecipient) recipient else other
    ts <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    genesis2: GenesisTransaction = GenesisTransaction.create(unleaser, ENOUGH_AMT, ts).right.get
    (lease, _) <- leaseAndCancelGeneratorP(master, recipient, master)
    fee2 <- smallFeeGen
    unleaseOtherOrRecipient = LeaseCancelTransaction.create(unleaser, lease.id, fee2, ts + 1).right.get
  } yield (genesis, genesis2, lease, unleaseOtherOrRecipient)

  property("cannot cancel lease of another sender after allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(Gen.oneOf(true, false).flatMap(cancelLeaseOfAnotherSender), timestampGen suchThat (_ > allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, genesis2, lease, unleaseOtherOrRecipient), blockTime) =>
        assertDiffEi(Seq(TestBlock(Seq(genesis, genesis2, lease))), TestBlock.create(blockTime, Seq(unleaseOtherOrRecipient)), settings) { totalDiffEi =>
          totalDiffEi should produce("LeaseTransaction was leased by other sender")
        }
    }
  }

  property("can cancel lease of another sender and acquire leasing power before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseOfAnotherSender(unleaseByRecipient = false), timestampGen retryUntil (_ < allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, genesis2, lease, unleaseOther), blockTime) =>
        assertDiffAndState(Seq(TestBlock(Seq(genesis, genesis2, lease))), TestBlock.create(blockTime, Seq(unleaseOther)), settings) { case (totalDiff, newState) =>
          totalDiff.txsDiff.portfolios.get(lease.sender) shouldBe None
          total(totalDiff.txsDiff.portfolios(lease.recipient.asInstanceOf[Account]).leaseInfo) shouldBe -lease.amount
          total(totalDiff.txsDiff.portfolios(unleaseOther.sender).leaseInfo) shouldBe lease.amount
        }
    }
  }

  property("if recipient cancels lease, it doesn't change leasing component of mining power before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseOfAnotherSender(unleaseByRecipient = true), timestampGen retryUntil (_ < allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, genesis2, lease, unleaseRecipient), blockTime) =>
        assertDiffAndState(Seq(TestBlock(Seq(genesis, genesis2, lease))), TestBlock.create(blockTime, Seq(unleaseRecipient)), settings) { case (totalDiff, newState) =>
          totalDiff.txsDiff.portfolios.get(lease.sender) shouldBe None
          total(totalDiff.txsDiff.portfolios(unleaseRecipient.sender).leaseInfo) shouldBe 0
        }
    }
  }
}

