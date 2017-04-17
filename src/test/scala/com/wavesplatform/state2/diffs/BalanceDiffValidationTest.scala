package com.wavesplatform.state2.diffs

import com.wavesplatform.TransactionGen
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
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

  ignore("disallows lease overflow") {

  }

}
