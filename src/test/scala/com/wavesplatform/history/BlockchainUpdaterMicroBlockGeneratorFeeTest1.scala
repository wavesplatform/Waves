package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import org.scalacheck.{Gen, Shrink}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.{GenesisTransaction, PaymentTransaction}

class BlockchainUpdaterMicroBlockGeneratorFeeTest1 extends PropSpec with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  type Setup = (GenesisTransaction, PaymentTransaction, PaymentTransaction)
  val preconditionsAndPayments: Gen[Setup] = for {
    sender <- accountGen
    recipient <- accountGen
    fee <- smallFeeGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(sender, ENOUGH_AMT, ts).right.get
    payment: PaymentTransaction <- paymentGeneratorP(sender, recipient)
    generatorPaymentOnFee: PaymentTransaction = PaymentTransaction.create(defaultSigner, recipient, payment.fee, fee, ts + 1).right.get
  } yield (genesis, payment, generatorPaymentOnFee)

  property("block generator can spend fee after transaction before fork") {
    scenario(preconditionsAndPayments,
      DefaultBlockchainSettings.copy(functionalitySettings = TestFunctionalitySettings.Enabled.copy(applyMinerFeeWithTransactionAfter = Long.MaxValue))) { case (domain, (genesis, somePayment, generatorPaymentOnFee)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(generatorPaymentOnFee, somePayment)))
      all(blocks.map(block => domain.blockchainUpdater.processBlock(block))) shouldBe 'right
    }
  }

  property("block generator can't spend fee after transaction after fork") {
    scenario(preconditionsAndPayments, DefaultBlockchainSettings.copy(functionalitySettings = TestFunctionalitySettings.Enabled.copy(applyMinerFeeWithTransactionAfter = 0))) { case (domain, (genesis, somePayment, generatorPaymentOnFee)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(generatorPaymentOnFee, somePayment)))
      blocks.init.foreach(block => domain.blockchainUpdater.processBlock(block).explicitGet())
      domain.blockchainUpdater.processBlock(blocks.last) should produce("unavailable funds")
    }
  }
}
