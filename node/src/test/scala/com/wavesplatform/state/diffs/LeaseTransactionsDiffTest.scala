package com.wavesplatform.state.diffs

import cats._
import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithDomain
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain._
import com.wavesplatform.it.util.DoubleExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.TestFunctionalitySettings
import com.wavesplatform.state._
import com.wavesplatform.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{GenesisTransaction, TxVersion}
import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.PropSpec
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class LeaseTransactionsDiffTest extends PropSpec with PropertyChecks with WithDomain with TransactionGen with NoShrink {

  private val allowMultipleLeaseCancelTransactionUntilTimestamp = Long.MaxValue / 2
  private val settings =
    TestFunctionalitySettings.Enabled.copy(lastTimeBasedForkParameter = allowMultipleLeaseCancelTransactionUntilTimestamp)

  def total(l: LeaseBalance): Long = l.in - l.out

  property("can lease/cancel lease preserving waves invariant") {

    val sunnyDayLeaseLeaseCancel: Gen[(GenesisTransaction, LeaseTransaction, LeaseCancelTransaction)] = for {
      master    <- accountGen
      recipient <- accountGen suchThat (_ != master)
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      (lease, unlease) <- leaseAndCancelGeneratorP(master, recipient.toAddress)
    } yield (genesis, lease, unlease)

    forAll(sunnyDayLeaseLeaseCancel) {
      case (genesis, lease, leaseCancel) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(lease))) {
          case (totalDiff, _) =>
            val totalPortfolioDiff = Monoid.combineAll(totalDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            total(totalPortfolioDiff.lease) shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)
        }

        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, lease))), TestBlock.create(Seq(leaseCancel))) {
          case (totalDiff, _) =>
            val totalPortfolioDiff = Monoid.combineAll(totalDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            total(totalPortfolioDiff.lease) shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.foreach(_ shouldBe 0)
        }
    }
  }

  private val repeatedCancelAllowed   = Gen.choose(0, allowMultipleLeaseCancelTransactionUntilTimestamp - 1)
  private val repeatedCancelForbidden = Gen.choose(allowMultipleLeaseCancelTransactionUntilTimestamp + 1, Long.MaxValue)

  def cancelLeaseTwice(ts: Long): Gen[(GenesisTransaction, TransferTransaction, LeaseTransaction, LeaseCancelTransaction, LeaseCancelTransaction)] =
    for {
      master   <- accountGen
      recpient <- accountGen suchThat (_ != master)
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      (lease, unlease) <- leaseAndCancelGeneratorP(master, recpient.toAddress, ts)
      fee2             <- smallFeeGen
      unlease2         <- createLeaseCancel(master, lease.id(), fee2, ts + 1)
      // ensure recipient has enough effective balance
      payment <- wavesTransferGeneratorP(ts, master, recpient.toAddress) suchThat (_.amount > lease.amount)
    } yield (genesis, payment, lease, unlease, unlease2)

  private val disallowCancelTwice = for {
    ts                                           <- repeatedCancelForbidden
    (genesis, payment, lease, unlease, unlease2) <- cancelLeaseTwice(ts)
  } yield (Seq(TestBlock.create(ts, Seq(genesis, payment, lease, unlease))), TestBlock.create(ts, Seq(unlease2)))

  property("cannot cancel lease twice after allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(disallowCancelTwice) {
      case (preconditions, block) =>
        assertDiffEi(preconditions, block, settings) { totalDiffEi =>
          totalDiffEi should produce("Cannot cancel already cancelled lease")
        }
    }
  }

  private val allowCancelTwice = for {
    ts                                           <- repeatedCancelAllowed
    (genesis, payment, lease, unlease, unlease2) <- cancelLeaseTwice(ts)
  } yield (Seq(TestBlock.create(ts, Seq(genesis, payment, lease, unlease))), TestBlock.create(ts, Seq(unlease2)))

  property("can cancel lease twice before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(allowCancelTwice) {
      case (preconditions, block) =>
        assertDiffEi(preconditions, block, settings) { totalDiffEi =>
          totalDiffEi.explicitGet()
        }
    }
  }

  property("cannot lease more than actual balance(cannot lease forward)") {
    val setup: Gen[(GenesisTransaction, LeaseTransaction, LeaseTransaction, Long)] = for {
      master    <- accountGen
      recipient <- accountGen suchThat (_ != master)
      forward   <- accountGen suchThat (!Set(master, recipient).contains(_))
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      (lease, _)        <- leaseAndCancelGeneratorP(master, recipient.toAddress, ts)
      (leaseForward, _) <- leaseAndCancelGeneratorP(recipient, forward.toAddress, ts)
    } yield (genesis, lease, leaseForward, ts)

    forAll(setup) {
      case (genesis, lease, leaseForward, ts) =>
        assertDiffEi(Seq(TestBlock.create(ts, Seq(genesis, lease))), TestBlock.create(ts, Seq(leaseForward)), settings) { totalDiffEi =>
          totalDiffEi should produce("Cannot lease more than own")
        }
    }
  }

  def cancelLeaseOfAnotherSender(
      unleaseByRecipient: Boolean,
      timestampGen: Gen[Long]
  ): Gen[(GenesisTransaction, GenesisTransaction, LeaseTransaction, LeaseCancelTransaction, Long)] =
    for {
      master    <- accountGen
      recipient <- accountGen suchThat (_ != master)
      other     <- accountGen suchThat (_ != recipient)
      unleaser = if (unleaseByRecipient) recipient else other
      ts <- timestampGen
      genesis: GenesisTransaction  = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(unleaser.toAddress, ENOUGH_AMT, ts).explicitGet()
      (lease, _)              <- leaseAndCancelGeneratorP(master, recipient.toAddress, ts)
      fee2                    <- smallFeeGen
      unleaseOtherOrRecipient <- createLeaseCancel(unleaser, lease.id(), fee2, ts + 1)
    } yield (genesis, genesis2, lease, unleaseOtherOrRecipient, ts)

  property("cannot cancel lease of another sender after allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(Gen.oneOf(true, false).flatMap(v => cancelLeaseOfAnotherSender(v, repeatedCancelForbidden))) {
      case (genesis, genesis2, lease, unleaseOtherOrRecipient, blockTime) =>
        assertDiffEi(
          Seq(TestBlock.create(blockTime, Seq(genesis, genesis2, lease))),
          TestBlock.create(blockTime, Seq(unleaseOtherOrRecipient)),
          settings
        ) { totalDiffEi =>
          totalDiffEi should produce("LeaseTransaction was leased by other sender")
        }
    }
  }

  property("can cancel lease of another sender and acquire leasing power before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseOfAnotherSender(unleaseByRecipient = false, repeatedCancelAllowed)) {
      case (genesis, genesis2, lease, unleaseOther, blockTime) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, genesis2, lease))), TestBlock.create(blockTime, Seq(unleaseOther)), settings) {
          case (totalDiff, _) =>
            totalDiff.portfolios.get(lease.sender.toAddress) shouldBe None
            total(totalDiff.portfolios(lease.recipient.asInstanceOf[Address]).lease) shouldBe -lease.amount
            total(totalDiff.portfolios(unleaseOther.sender.toAddress).lease) shouldBe lease.amount
        }
    }
  }

  property("if recipient cancels lease, it doesn't change leasing component of mining power before allowMultipleLeaseCancelTransactionUntilTimestamp") {
    forAll(cancelLeaseOfAnotherSender(unleaseByRecipient = true, repeatedCancelAllowed)) {
      case (genesis, genesis2, lease, unleaseRecipient, blockTime) =>
        assertDiffAndState(
          Seq(TestBlock.create(blockTime, Seq(genesis, genesis2, lease))),
          TestBlock.create(blockTime, Seq(unleaseRecipient)),
          settings
        ) {
          case (totalDiff, _) =>
            totalDiff.portfolios.get(lease.sender.toAddress) shouldBe None
            total(totalDiff.portfolios(unleaseRecipient.sender.toAddress).lease) shouldBe 0
        }
    }
  }

  property(s"can pay for cancel lease from the returning funds (before and after ${BlockchainFeatures.BlockV5})") {
    val scenario =
      for {
        master    <- accountGen
        recipient <- accountGen suchThat (_ != master)
        fee       <- smallFeeGen
        amount    <- positiveLongGen
        ts        <- timestampGen
        genesis = GenesisTransaction.create(master.toAddress, fee + amount, ts).explicitGet()
        lease       <- createLease(master, amount, fee, ts, recipient.toAddress)
        leaseCancel <- createLeaseCancel(master, lease.id(), fee, ts + 1)
      } yield (genesis, lease, leaseCancel, ts + 2)

    forAll(scenario) {
      case (genesis, lease, leaseCancel, ts) =>
        val beforeFailedTxs = TestFunctionalitySettings.Enabled
        val afterFailedTxs = beforeFailedTxs.copy(
          preActivatedFeatures = beforeFailedTxs.preActivatedFeatures + (BlockchainFeatures.BlockV5.id -> 0)
        )

        assertDiffEi(Seq(TestBlock.create(ts, Seq(genesis, lease))), TestBlock.create(ts + 1, Seq(leaseCancel)), beforeFailedTxs) { ei =>
          ei.explicitGet()
        }

        assertDiffEi(Seq(TestBlock.create(ts, Seq(genesis, lease))), TestBlock.create(ts + 1, Seq(leaseCancel)), afterFailedTxs) { ei =>
          ei.explicitGet()
        }
    }
  }

  private val totalBalance = 1000.waves
  private val scenario: Gen[(GenesisTransaction, LeaseTransaction)] = for {
    sender    <- accountGen
    recipient <- accountGen
    fee       <- smallFeeGen
  } yield (
    GenesisTransaction.create(sender.toAddress, totalBalance, ntpTime.getTimestamp()).explicitGet(),
    LeaseTransaction.selfSigned(TxVersion.V1, sender, recipient.toAddress, totalBalance, fee, ntpTime.getTimestamp()).explicitGet()
  )

  property(s"fee is not required prior to ${BlockchainFeatures.SynchronousCalls}") {
    forAll(scenario) {
      case (gt, lt) =>
        withDomain(domainSettingsWithFeatures(BlockchainFeatures.SynchronousCalls -> 5)) { d =>
          d.appendBlock(gt)
          d.appendBlock(lt)
        }
    }
  }

  property(s"fee is not required once ${BlockchainFeatures.SynchronousCalls} is activated") {
    forAll(scenario) {
      case (gt, lt) =>
        withDomain(domainSettingsWithFeatures(BlockchainFeatures.SynchronousCalls -> 1)) { d =>
          d.appendBlock(gt)
          d.blockchainUpdater.processBlock(d.createBlock(Block.PlainBlockVersion, Seq(lt))) should produce("Cannot lease more than own")
        }
    }
  }
}
