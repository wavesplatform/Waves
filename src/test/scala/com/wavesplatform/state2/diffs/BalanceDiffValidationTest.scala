package com.wavesplatform.state2.diffs

import com.wavesplatform.TransactionGen
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.{GenesisTransaction, PaymentTransaction}

class BalanceDiffValidationTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

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


    forAll(preconditionsAndPayment, accountGen) { case ((gen1, gen2, transfer1, transfer2), miner) =>
      assertDiffEi(Seq(TestBlock(Seq(gen1, gen2, transfer1))), TestBlock(Seq(transfer2), miner)) { blockDiffEi =>
        blockDiffEi should produce("Transaction application leads to negative balance")
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
      assertDiffEi(Seq(TestBlock(Seq(gen1, gen2, l1))), TestBlock(Seq(l2)))(totalDiffEi =>
        totalDiffEi should produce("Transaction application leads to negative balance"))
    }
  }

}
