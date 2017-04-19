package com.wavesplatform.state2.diffs

import com.wavesplatform.TransactionGen
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{GenesisTransaction, PaymentTransaction}

class CommonValidationTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  property("disallows double spending") {
    val preconditionsAndPayment: Gen[(GenesisTransaction, PaymentTransaction)] = for {
      master <- accountGen
      recipient <- otherAccountGen(candidate = master)
      ts <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      transfer: PaymentTransaction <- paymentGeneratorP(master, recipient)
    } yield (genesis, transfer)

    forAll(preconditionsAndPayment) { case ((genesis, transfer)) =>
      assertDiffEi(Seq(TestBlock(Seq(genesis, transfer))), TestBlock(Seq(transfer))) { blockDiffEi =>
        blockDiffEi should produce("Tx with such id aready present")
      }

      assertDiffEi(Seq(TestBlock(Seq(genesis))), TestBlock(Seq(transfer, transfer))) { blockDiffEi =>
        blockDiffEi should produce("Tx with such id aready present")
      }
    }
  }
}
