package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.GenesisTransaction
import scorex.transaction.assets.TransferTransaction

class BlockchainUpdaterGeneratorFeeNextBlockOrMicroBlockTest extends PropSpec
  with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {

  type Setup = (GenesisTransaction, TransferTransaction, TransferTransaction, TransferTransaction)

  val preconditionsAndPayments: Gen[Setup] = for {
    sender <- accountGen
    recipient <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(sender, ENOUGH_AMT, ts).right.get
    somePayment: TransferTransaction = createWavesTransfer(sender, recipient, 1, 10, ts + 1).right.get
    // generator has enough balance for this transaction if gets fee for block before applying it
    generatorPaymentOnFee: TransferTransaction = createWavesTransfer(defaultSigner, recipient, 11, 1, ts + 2).right.get
    someOtherPayment: TransferTransaction = createWavesTransfer(sender, recipient, 1, 1, ts + 3).right.get
  } yield (genesis, somePayment, generatorPaymentOnFee, someOtherPayment)

  property("generator should get fees before applying block before applyMinerFeeWithTransactionAfter in two blocks") {
    scenario(preconditionsAndPayments, DefaultWavesSettings) { case (domain: Domain, (genesis, somePayment, generatorPaymentOnFee, someOtherPayment)) =>
      val blocks = chainBlocks(Seq(Seq(genesis, somePayment), Seq(generatorPaymentOnFee, someOtherPayment)))
      all(blocks.map(block => domain.blockchainUpdater.processBlock(block))) shouldBe 'right
    }
  }

  property("generator should get fees before applying block before applyMinerFeeWithTransactionAfter in block + micro") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, somePayment, generatorPaymentOnFee, someOtherPayment)) =>
      val (block, microBlocks) =
        chainBaseAndMicro(randomSig, genesis, Seq(Seq(somePayment), Seq(generatorPaymentOnFee, someOtherPayment)))
      domain.blockchainUpdater.processBlock(block).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks(0)) shouldBe 'right
      domain.blockchainUpdater.processMicroBlock(microBlocks(1)) should produce("unavailable funds")
    }
  }

  property("generator should get fees after applying every transaction after applyMinerFeeWithTransactionAfter in two blocks") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, somePayment, generatorPaymentOnFee, someOtherPayment)) =>
      val blocks = chainBlocks(Seq(Seq(genesis, somePayment), Seq(generatorPaymentOnFee, someOtherPayment)))
      domain.blockchainUpdater.processBlock(blocks(0)) shouldBe 'right
      domain.blockchainUpdater.processBlock(blocks(1)) should produce("unavailable funds")
    }
  }

  property("generator should get fees after applying every transaction after applyMinerFeeWithTransactionAfter in block + micro") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, somePayment, generatorPaymentOnFee, someOtherPayment)) =>
      val (block, microBlocks) =
        chainBaseAndMicro(randomSig, genesis, Seq(Seq(somePayment), Seq(generatorPaymentOnFee, someOtherPayment)))
      domain.blockchainUpdater.processBlock(block).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks(0)).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks(1)) should produce("unavailable funds")
    }
  }
}
