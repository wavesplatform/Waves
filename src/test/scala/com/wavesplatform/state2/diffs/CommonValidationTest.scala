package com.wavesplatform.state2.diffs

import com.wavesplatform.TransactionGen
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction.{GenesisTransaction, PaymentTransaction}

class CommonValidationTest extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("disallows double spending") {
    val preconditionsAndPayment: Gen[(GenesisTransaction, PaymentTransaction)] = for {
      master <- accountGen
      recipient <- otherAccountGen(candidate = master)
      ts <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
      transfer: PaymentTransaction <- paymentGeneratorP(master, recipient)
    } yield (genesis, transfer)

    forAll(preconditionsAndPayment) { case ((genesis, transfer)) =>
      assertDiffEi(Seq(TestBlock.create(Seq(genesis, transfer))), TestBlock.create(Seq(transfer))) { blockDiffEi =>
        blockDiffEi should produce("Tx with such id aready present")
      }

      assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transfer, transfer))) { blockDiffEi =>
        blockDiffEi should produce("Tx with such id aready present")
      }
    }
  }
}
