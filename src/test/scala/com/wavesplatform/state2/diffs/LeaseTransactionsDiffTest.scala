package com.wavesplatform.state2.diffs

import cats._
import com.wavesplatform.state2._
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.account.{Account, PrivateKeyAccount}
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.{GenesisTransaction, PaymentTransaction, TransactionGen}

class LeaseTransactionsDiffTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  private val allowMultipleLeaseCancelTransactionUntilTimestamp = TestFunctionalitySettings.Enabled.allowMultipleLeaseCancelTransactionUntilTimestamp

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
        totalPortfolioDiff.leaseInfo.leaseIn - totalPortfolioDiff.leaseInfo.leaseOut shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)

        val snapshot = EffectiveBalanceSnapshot(lease.recipient.asInstanceOf[Account], 2, 0, lease.amount)
        totalDiff.effectiveBalanceSnapshots should contain(snapshot)
      }

      assertDiffAndState(Seq(TestBlock(Seq(genesis, lease))), TestBlock(Seq(leaseCancel), miner)) { case (totalDiff, newState) =>
        val totalPortfolioDiff = Monoid.combineAll(totalDiff.txsDiff.portfolios.values)
        totalPortfolioDiff.balance shouldBe 0
        totalPortfolioDiff.leaseInfo.leaseIn - totalPortfolioDiff.leaseInfo.leaseOut shouldBe 0
        totalPortfolioDiff.effectiveBalance shouldBe 0
        totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)

        val snapshot = EffectiveBalanceSnapshot(lease.recipient.asInstanceOf[Account], 2, lease.amount, 0)
        totalDiff.effectiveBalanceSnapshots should contain(snapshot)

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
    payment <- paymentGeneratorP(ts, master, recpient) suchThat (_.amount > lease.amount)
  } yield (genesis, payment, lease, unlease, unlease2)

  property("cannot cancel lease twice after allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseTwice, timestampGen suchThat (_ > allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, payment, lease, leaseCancel, leaseCancel2), blockTime) =>
        assertDiffEi(Seq(TestBlock(Seq(genesis, payment, lease, leaseCancel))), TestBlock.create(blockTime, Seq(leaseCancel2))) { totalDiffEi =>
          totalDiffEi should produce("Cannot cancel already cancelled lease")
        }
    }
  }

  property("can cancel lease twice before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseTwice, timestampGen suchThat (_ < allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, payment, lease, leaseCancel, leaseCancel2), blockTime) =>
        assertDiffEi(Seq(TestBlock(Seq(genesis, payment, lease, leaseCancel))), TestBlock.create(blockTime, Seq(leaseCancel2))) { totalDiffEi =>
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
      assertDiffEi(Seq(TestBlock(Seq(genesis, lease))), TestBlock(Seq(leaseForward))) { totalDiffEi =>
        totalDiffEi should produce("Cannot lease more than own")
      }
    }
  }

  val cancelLeaseOfAnotherSender: Gen[(GenesisTransaction, GenesisTransaction, LeaseTransaction, LeaseCancelTransaction)] = for {
    master <- accountGen
    recipient <- accountGen suchThat (_ != master)
    otherOrRecipient <- otherAccountGen(candidate = recipient)
    ts <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    genesis2: GenesisTransaction = GenesisTransaction.create(otherOrRecipient, ENOUGH_AMT, ts).right.get
    (lease, _) <- leaseAndCancelGeneratorP(master, recipient, master)
    fee2 <- smallFeeGen
    unleaseOtherOrRecipient = LeaseCancelTransaction.create(otherOrRecipient, lease.id, fee2, ts + 1).right.get
  } yield (genesis, genesis2, lease, unleaseOtherOrRecipient)

  property("cannot cancel lease of another sender after allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseOfAnotherSender, timestampGen suchThat (_ > allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, genesis2, lease, unleaseOtherOrRecipient), blockTime) =>
        assertDiffEi(Seq(TestBlock(Seq(genesis, genesis2, lease))), TestBlock.create(blockTime, Seq(unleaseOtherOrRecipient))) { totalDiffEi =>
          totalDiffEi should produce("LeaseTransaction was leased by other sender")
        }
    }
  }

  property("can cancel lease of another sender and acquire leasing power before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseOfAnotherSender suchThat { case (_, _, lease, unlease) => lease.recipient.asInstanceOf[Account] != unlease.sender.toAccount },
      timestampGen suchThat (_ < allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, genesis2, lease, unleaseOther), blockTime) =>
        assertDiffAndState(Seq(TestBlock(Seq(genesis, genesis2, lease))), TestBlock.create(blockTime, Seq(unleaseOther))) { case (totalDiff, newState) =>
          totalDiff.txsDiff.portfolios.get(lease.sender) shouldBe None
          total(totalDiff.txsDiff.portfolios(lease.recipient.asInstanceOf[Account]).leaseInfo) shouldBe -lease.amount
          total(totalDiff.txsDiff.portfolios(unleaseOther.sender).leaseInfo) shouldBe lease.amount
        }
    }
  }

  property("if recipient cancels lease, it doesn't change leaseing component of mining power before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseOfAnotherSender suchThat { case (_, _, lease, unlease) => lease.recipient.asInstanceOf[Account] == unlease.sender.toAccount }
      , timestampGen suchThat (_ < allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, genesis2, lease, unleaseRecipient), blockTime) =>
        assertDiffAndState(Seq(TestBlock(Seq(genesis, genesis2, lease))), TestBlock.create(blockTime, Seq(unleaseRecipient))) { case (totalDiff, newState) =>
          totalDiff.txsDiff.portfolios.get(lease.sender) shouldBe None
          total(totalDiff.txsDiff.portfolios(unleaseRecipient.sender).leaseInfo) shouldBe 0
        }
    }
  }

  property("validation of 9QqKxGT8s8AYQfh2UBwT3s9Y87nuZwfcJpLxJJ823DgP") {
    val setup = for {
      master <- accountGen
      ts <- timestampGen suchThat (_ < allowMultipleLeaseCancelTransactionUntilTimestamp)
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      aW3P9g9 <- accountGen
      aW3PE4T <- accountGen
      aW3PBXV <- accountGen
      amt = 576100000000L
      amtOther = 500000000000L
      fee = 1000000L
      initialPayment = PaymentTransaction.create(master, aW3P9g9, amt + (fee * 9), fee, ts).right.get
      initialPayment2 = PaymentTransaction.create(master, aW3PE4T, amt + (fee * 9), fee, ts).right.get
      lease3PE3PB = LeaseTransaction.create(aW3PE4T, amtOther, fee, ts, aW3PBXV).right.get
      l1 = LeaseTransaction.create(aW3P9g9, amt, fee, ts, aW3PE4T).right.get
      l1c1 = LeaseCancelTransaction.create(aW3P9g9, l1.id, fee, ts).right.get
      l1c2 = LeaseCancelTransaction.create(aW3P9g9, l1.id, fee, ts + 1).right.get
      l2 = LeaseTransaction.create(aW3P9g9, amt, fee, ts + 2, aW3PE4T).right.get
      l3 = LeaseTransaction.create(aW3P9g9, amt, fee, ts + 3, aW3PBXV).right.get
      l2c1 = LeaseCancelTransaction.create(aW3P9g9, lease3PE3PB.id, fee, ts + 4).right.get
      l2c2 = LeaseCancelTransaction.create(aW3P9g9, lease3PE3PB.id, fee, ts + 5).right.get
      l2c3 = LeaseCancelTransaction.create(aW3P9g9, lease3PE3PB.id, fee, ts + 6).right.get
      transfer = TransferTransaction.create(None, aW3P9g9, aW3PE4T, amt, ts, None, fee, Array.empty).right.get
    } yield (aW3P9g9, aW3PE4T, aW3PBXV, genesis, initialPayment, initialPayment2, lease3PE3PB, l1, l1c1, l1c2, l2, l3, l2c1, l2c2, l2c3, transfer)

    forAll(setup) { case (aW3P9g9, aW3PE4T, aW3PBXV, genesis, initialPayment, initialPayment2, lease3PE3PB, l1, l1c1, l1c2, l2, l3, l2c1, l2c2, l2c3, transfer) => {
      assertDiffAndState(Seq(
        TestBlock(Seq(genesis, initialPayment, initialPayment2, lease3PE3PB)),
        TestBlock(Seq(l1, l1c1, l1c2, l2, l3, l2c1, l2c2, l2c3))
      ), TestBlock(Seq(transfer))) { case (_, newState) =>
        // it's valid
      }
    }
    }
  }

}

