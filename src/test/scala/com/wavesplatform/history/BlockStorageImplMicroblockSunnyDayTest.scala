package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import org.scalacheck.{Gen, Shrink}
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.transaction._

class BlockStorageImplMicroblockSunnyDayTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndPayments: Gen[(GenesisTransaction, PaymentTransaction, PaymentTransaction, PaymentTransaction)] = for {
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
    forAll(preconditionsAndPayments) { case ((genesis, payment, finalPayment, conflictingFinalPayment)) =>
      val fp = setup()
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment), Seq(finalPayment), Seq(conflictingFinalPayment)))
      blocks.init.foreach(block => fp.blockchainUpdater.processBlock(block).explicitGet())
      fp.blockchainUpdater.processBlock(blocks.last) should produce("unavailable funds")
    }
  }
  property("all txs in one block: B0 <- B0m1 <- B0m2 <- B0m3!") {
    forAll(preconditionsAndPayments) { case ((genesis, payment, finalPayment, conflictingFinalPayment)) =>
      val fp = setup()
      val (block, microBlocks) = chainBaseAndMicro(TestBlock.randomOfLength(Block.BlockIdLength), genesis, Seq(payment, finalPayment, conflictingFinalPayment))
      fp.blockchainUpdater.processBlock(block).explicitGet()
      fp.blockchainUpdater.processMicroBlock(microBlocks(0)).explicitGet()
      fp.blockchainUpdater.processMicroBlock(microBlocks(1)).explicitGet()
      fp.blockchainUpdater.processMicroBlock(microBlocks(2)) should produce("unavailable funds")
    }
  }
  ignore("block references microBlock: B0 <- B1 <- B1m1 <- B2!") {}
  ignore("discards liquid block: B0 <- B0m1 <- B0m2; B0 <- B1") {} // all works
  ignore("discards microBlocks: B0 <- B1 <- B1m1; B1 <- B2") {} // all works
  ignore("discards some of microBlocks: B0 <- B0m1 <- B0m2; B0m1 <- B1") {} // all works
}
