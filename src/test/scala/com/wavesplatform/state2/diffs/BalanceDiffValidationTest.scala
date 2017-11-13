package com.wavesplatform.state2.diffs

import com.wavesplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.{GenesisTransaction, PaymentTransaction}

class BalanceDiffValidationTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("disallows overflow") {
    val preconditionsAndPayment: Gen[(GenesisTransaction, GenesisTransaction, PaymentTransaction, PaymentTransaction)] = for {
      master <- accountGen
      master2 <- accountGen
      recipient <- otherAccountGen(candidate = master)
      ts <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(master, Long.MaxValue - 1, ts).right.get
      gen2: GenesisTransaction = GenesisTransaction.create(master2, Long.MaxValue - 1, ts).right.get
      fee <- smallFeeGen
      amount <- Gen.choose(Long.MaxValue / 2, Long.MaxValue - fee - 1)
      transfer1 = PaymentTransaction.create(master, recipient, amount, fee, ts).right.get
      transfer2 = PaymentTransaction.create(master2, recipient, amount, fee, ts).right.get
    } yield (gen1, gen2, transfer1, transfer2)


    forAll(preconditionsAndPayment) { case ((gen1, gen2, transfer1, transfer2)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, transfer1))), TestBlock.create(Seq(transfer2))) { blockDiffEi =>
        blockDiffEi should produce("negative waves balance")
      }
    }
  }

  property("disallows lease overflow") {
    val setup = for {
      master1 <- accountGen
      master2 <- accountGen
      recipient <- accountGen
      ts <- timestampGen
      gen1 = GenesisTransaction.create(master1, Long.MaxValue - 1, ts).right.get
      gen2 = GenesisTransaction.create(master2, Long.MaxValue - 1, ts).right.get
      fee <- smallFeeGen
      amt1 <- Gen.choose(Long.MaxValue / 2 + 1, Long.MaxValue - 1 - fee)
      amt2 <- Gen.choose(Long.MaxValue / 2 + 1, Long.MaxValue - 1 - fee)
      l1 = LeaseTransaction.create(master1, amt1, fee, ts, recipient).right.get
      l2 = LeaseTransaction.create(master2, amt2, fee, ts, recipient).right.get
    } yield (gen1, gen2, l1, l2)

    forAll(setup) { case (gen1, gen2, l1, l2) =>
      assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, l1))), TestBlock.create(Seq(l2)))(totalDiffEi =>
        totalDiffEi should produce("negative effective balance"))
    }
  }


  val ownLessThatLeaseOut: Gen[(GenesisTransaction, PaymentTransaction, LeaseTransaction, LeaseTransaction, PaymentTransaction)] = for {
    master <- accountGen
    alice <- accountGen
    bob <- accountGen
    cooper <- accountGen
    ts <- positiveIntGen
    amt <- positiveLongGen
    fee <- smallFeeGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    masterTransfersToAlice: PaymentTransaction = PaymentTransaction.create(master, alice, amt, fee, ts).right.get
    (aliceLeasesToBob, _) <- leaseAndCancelGeneratorP(alice, bob, alice) suchThat (_._1.amount < amt)
    (masterLeasesToAlice, _) <- leaseAndCancelGeneratorP(master, alice, master) suchThat (_._1.amount > aliceLeasesToBob.amount)
    transferAmt <- Gen.choose(amt - fee - aliceLeasesToBob.amount, amt - fee)
    aliceTransfersMoreThanOwnsMinusLeaseOut = PaymentTransaction.create(alice, cooper, transferAmt, fee, ts).right.get

  } yield (genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut)


  property("can transfer more than own-leaseOut before allow-leased-balance-transfer-until") {
    val allowTransferLeasedBalanceUntil = Long.MaxValue / 2
    val settings = TestFunctionalitySettings.Enabled.copy(allowLeasedBalanceTransferUntil = allowTransferLeasedBalanceUntil)

    forAll(ownLessThatLeaseOut, timestampGen retryUntil (_ < allowTransferLeasedBalanceUntil)) {
      case ((genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut), blockTime) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice))),
          TestBlock.create(blockTime, Seq(aliceTransfersMoreThanOwnsMinusLeaseOut)),
          settings) { totalDiffEi =>
          totalDiffEi shouldBe 'right
        }
    }
  }

  property("cannot transfer more than own-leaseOut after allow-leased-balance-transfer-until") {
    val allowTransferLeasedBalanceUntil = Long.MaxValue / 2
    val settings = TestFunctionalitySettings.Enabled.copy(allowLeasedBalanceTransferUntil = allowTransferLeasedBalanceUntil)

    forAll(ownLessThatLeaseOut, timestampGen retryUntil (_ > allowTransferLeasedBalanceUntil)) {
      case ((genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice, aliceTransfersMoreThanOwnsMinusLeaseOut), blockTime) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, masterTransfersToAlice, aliceLeasesToBob, masterLeasesToAlice))),
          TestBlock.create(blockTime, Seq(aliceTransfersMoreThanOwnsMinusLeaseOut)),
          settings) { totalDiffEi =>
          totalDiffEi should produce("leased being more than own")
        }
    }
  }
}
