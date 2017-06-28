package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import org.scalacheck.{Gen, Shrink}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction._

class BlockStorageImplMicroblockSunnyDayTest extends PropSpec with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  type Setup = (GenesisTransaction, PaymentTransaction, PaymentTransaction, PaymentTransaction)
  val preconditionsAndPayments: Gen[Setup] = for {
    master <- accountGen
    recipient <- accountGen
    finalRecipient <- accountGen
    ts <- positiveIntGen
    fee <- smallFeeGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    payment: PaymentTransaction <- paymentGeneratorP(master, recipient)
    finalPayment = PaymentTransaction.create(recipient, finalRecipient, payment.amount - fee - 1, fee, ts).right.get
    conflictingFinalPayment = PaymentTransaction.create(recipient, finalRecipient, payment.amount - fee - 1, fee, ts + 1).right.get
  } yield (genesis, payment, finalPayment, conflictingFinalPayment)

  property("all txs in different blocks: B0 <- B1 <- B2 <- B3!") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, finalPayment, conflictingFinalPayment)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment), Seq(finalPayment), Seq(conflictingFinalPayment)))
      blocks.init.foreach(block => domain.blockchainUpdater.processBlock(block).explicitGet())
      domain.blockchainUpdater.processBlock(blocks.last) should produce("unavailable funds")
    }
  }

  property("all txs in one block: B0 <- B0m1 <- B0m2 <- B0m3!") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, finalPayment, conflictingFinalPayment)) =>
      val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, Seq(payment, finalPayment, conflictingFinalPayment))
      domain.blockchainUpdater.processBlock(block).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks(0)).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks(1)).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks(2)) should produce("unavailable funds")
    }
  }


  property("block references microBlock: B0 <- B1 <- B1m1 <- B2!") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, finalPayment, conflictingFinalPayment)) =>
      val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, Seq(payment, finalPayment, conflictingFinalPayment))
      domain.blockchainUpdater.processBlock(block).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks(0)).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks(1)).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks(2)) should produce("unavailable funds")
    }
  }

  property("discards some of microBlocks: B0 <- B0m1 <- B0m2; B0m1 <- B1") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, finalPayment, conflictingFinalPayment)) =>
      val (block0, microBlocks0) = chainBaseAndMicro(randomSig, genesis, Seq(payment, finalPayment))
      val block1 = buildBlockOfTxs(microBlocks0.head.totalResBlockSig, Seq(conflictingFinalPayment))
      domain.blockchainUpdater.processBlock(block0).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks0(0)).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks0(1)).explicitGet()
      domain.blockchainUpdater.processBlock(block1) shouldBe 'right
    }
  }

  property("discards all microBlocks: B0 <- B1 <- B1m1; B1 <- B2") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, finalPayment, conflictingFinalPayment)) =>
      val block0 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microBlocks1) = chainBaseAndMicro(block0.uniqueId, payment, Seq(finalPayment))
      val block2 = buildBlockOfTxs(block1.uniqueId, Seq(conflictingFinalPayment))
      domain.blockchainUpdater.processBlock(block0).explicitGet()
      domain.blockchainUpdater.processBlock(block1).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks1.head).explicitGet()
      domain.blockchainUpdater.processBlock(block2) shouldBe 'right
    }
  }

  property("discards liquid block completely: B0 <- B0m1 <- B0m2; B0 <- B1") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, finalPayment, conflictingFinalPayment)) =>
      val (block0, microBlocks0) = chainBaseAndMicro(randomSig, genesis, Seq(payment, finalPayment))
      val block1 = buildBlockOfTxs(block0.uniqueId, Seq(payment, conflictingFinalPayment))
      domain.blockchainUpdater.processBlock(block0).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks0(0)).explicitGet()
      domain.blockchainUpdater.processMicroBlock(microBlocks0(1)).explicitGet()
      domain.blockchainUpdater.processBlock(block1) shouldBe 'right
    }
  }
}
