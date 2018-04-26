package com.wavesplatform.history

import com.wavesplatform.OldTransactionGen
import com.wavesplatform.state._
import com.wavesplatform.state.diffs._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.TransferTransaction

class BlockchainUpdaterGeneratorFeeSameBlockTest
    extends PropSpec
    with PropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with OldTransactionGen {

  type Setup = (GenesisTransaction, TransferTransaction, TransferTransaction)

  val preconditionsAndPayments: Gen[Setup] = for {
    sender    <- accountGen
    recipient <- accountGen
    fee       <- smallFeeGen
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(sender, ENOUGH_AMT, ts).right.get
    payment: TransferTransaction <- wavesTransferGeneratorP(sender, recipient)
    generatorPaymentOnFee: TransferTransaction = createWavesTransfer(defaultSigner, recipient, payment.fee, fee, ts + 1).right.get
  } yield (genesis, payment, generatorPaymentOnFee)

  property("block generator can spend fee after transaction before applyMinerFeeWithTransactionAfter") {
    scenario(preconditionsAndPayments, DefaultWavesSettings) {
      case (domain, (genesis, somePayment, generatorPaymentOnFee)) =>
        val blocks = chainBlocks(Seq(Seq(genesis), Seq(generatorPaymentOnFee, somePayment)))
        all(blocks.map(block => domain.blockchainUpdater.processBlock(block))) shouldBe 'right
    }
  }

  property("block generator can't spend fee after transaction after applyMinerFeeWithTransactionAfter") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) {
      case (domain, (genesis, somePayment, generatorPaymentOnFee)) =>
        val blocks = chainBlocks(Seq(Seq(genesis), Seq(generatorPaymentOnFee, somePayment)))
        blocks.init.foreach(block => domain.blockchainUpdater.processBlock(block).explicitGet())
        domain.blockchainUpdater.processBlock(blocks.last) should produce("unavailable funds")
    }
  }
}
