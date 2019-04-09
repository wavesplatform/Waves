package com.wavesplatform.state.diffs

import cats._
import com.wavesplatform.account.Address
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class LeaseTransactionsDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private val allowMultipleLeaseCancelTransactionUntilTimestamp = Long.MaxValue / 2
  private val settings =
    TestFunctionalitySettings.Enabled.copy(allowMultipleLeaseCancelTransactionUntilTimestamp = allowMultipleLeaseCancelTransactionUntilTimestamp)

  def total(l: LeaseBalance): Long = l.in - l.out

  property("can lease/cancel lease preserving waves invariant") {

    val sunnyDayLeaseLeaseCancel: Gen[(GenesisTransaction, LeaseTransaction, LeaseCancelTransaction)] = for {
      master    <- accountGen
      recipient <- accountGen suchThat (_ != master)
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      (lease, unlease) <- leaseAndCancelGeneratorP(master, recipient, master)
    } yield (genesis, lease, unlease)

    forAll(sunnyDayLeaseLeaseCancel) {
      case ((genesis, lease, leaseCancel)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(lease))) {
          case (totalDiff, newState) =>
            val totalPortfolioDiff = Monoid.combineAll(totalDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            total(totalPortfolioDiff.lease) shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)
        }

        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, lease))), TestBlock.create(Seq(leaseCancel))) {
          case (totalDiff, newState) =>
            val totalPortfolioDiff = Monoid.combineAll(totalDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            total(totalPortfolioDiff.lease) shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)
        }
    }
  }

  val cancelLeaseTwice: Gen[(GenesisTransaction, TransferTransactionV1, LeaseTransaction, LeaseCancelTransaction, LeaseCancelTransaction)] = for {
    master   <- accountGen
    recpient <- accountGen suchThat (_ != master)
    ts       <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    (lease, unlease) <- leaseAndCancelGeneratorP(master, recpient, master)
    fee2             <- smallFeeGen
    unlease2         <- createLeaseCancel(master, lease.id(), fee2, ts + 1)
    // ensure recipient has enough effective balance
    payment <- wavesTransferGeneratorP(master, recpient) suchThat (_.amount > lease.amount)
  } yield (genesis, payment, lease, unlease, unlease2)

  property("cannot cancel lease twice after allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseTwice, timestampGen retryUntil (_ > allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, payment, lease, leaseCancel, leaseCancel2), blockTime) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, payment, lease, leaseCancel))), TestBlock.create(blockTime, Seq(leaseCancel2)), settings) {
          totalDiffEi =>
            totalDiffEi should produce("Cannot cancel already cancelled lease")
        }
    }
  }

  property("can cancel lease twice before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseTwice, timestampGen retryUntil (_ < allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, payment, lease, leaseCancel, leaseCancel2), blockTime) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, payment, lease, leaseCancel))), TestBlock.create(blockTime, Seq(leaseCancel2)), settings) {
          totalDiffEi =>
            totalDiffEi shouldBe 'right
        }
    }
  }

  property("cannot lease more than actual balance(cannot lease forward)") {
    val setup: Gen[(GenesisTransaction, LeaseTransaction, LeaseTransaction)] = for {
      master    <- accountGen
      recipient <- accountGen suchThat (_ != master)
      forward   <- accountGen suchThat (!Set(master, recipient).contains(_))
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      (lease, _)        <- leaseAndCancelGeneratorP(master, recipient, master)
      (leaseForward, _) <- leaseAndCancelGeneratorP(recipient, forward, recipient)
    } yield (genesis, lease, leaseForward)

    forAll(setup) {
      case ((genesis, lease, leaseForward)) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, lease))), TestBlock.create(Seq(leaseForward)), settings) { totalDiffEi =>
          totalDiffEi should produce("Cannot lease more than own")
        }
    }
  }

  def cancelLeaseOfAnotherSender(
      unleaseByRecipient: Boolean): Gen[(GenesisTransaction, GenesisTransaction, LeaseTransaction, LeaseCancelTransaction)] =
    for {
      master    <- accountGen
      recipient <- accountGen suchThat (_ != master)
      other     <- accountGen suchThat (_ != recipient)
      unleaser = if (unleaseByRecipient) recipient else other
      ts <- timestampGen
      genesis: GenesisTransaction  = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(unleaser, ENOUGH_AMT, ts).explicitGet()
      (lease, _)              <- leaseAndCancelGeneratorP(master, recipient, master)
      fee2                    <- smallFeeGen
      unleaseOtherOrRecipient <- createLeaseCancel(unleaser, lease.id(), fee2, ts + 1)
    } yield (genesis, genesis2, lease, unleaseOtherOrRecipient)

  property("cannot cancel lease of another sender after allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(Gen.oneOf(true, false).flatMap(cancelLeaseOfAnotherSender),
           timestampGen retryUntil (_ > allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, genesis2, lease, unleaseOtherOrRecipient), blockTime) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, genesis2, lease))), TestBlock.create(blockTime, Seq(unleaseOtherOrRecipient)), settings) {
          totalDiffEi =>
            totalDiffEi should produce("LeaseTransaction was leased by other sender")
        }
    }
  }

  property("can cancel lease of another sender and acquire leasing power before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseOfAnotherSender(unleaseByRecipient = false), timestampGen retryUntil (_ < allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, genesis2, lease, unleaseOther), blockTime) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2, lease))), TestBlock.create(blockTime, Seq(unleaseOther)), settings) {
          case (totalDiff, newState) =>
            totalDiff.portfolios.get(lease.sender) shouldBe None
            total(totalDiff.portfolios(lease.recipient.asInstanceOf[Address]).lease) shouldBe -lease.amount
            total(totalDiff.portfolios(unleaseOther.sender).lease) shouldBe lease.amount
        }
    }
  }

  property("if recipient cancels lease, it doesn't change leasing component of mining power before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseOfAnotherSender(unleaseByRecipient = true), timestampGen retryUntil (_ < allowMultipleLeaseCancelTransactionUntilTimestamp)) {
      case ((genesis, genesis2, lease, unleaseRecipient), blockTime) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2, lease))), TestBlock.create(blockTime, Seq(unleaseRecipient)), settings) {
          case (totalDiff, newState) =>
            totalDiff.portfolios.get(lease.sender) shouldBe None
            total(totalDiff.portfolios(unleaseRecipient.sender).lease) shouldBe 0
        }
    }
  }
}
