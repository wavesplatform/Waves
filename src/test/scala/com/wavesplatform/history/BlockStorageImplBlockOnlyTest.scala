package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import org.scalacheck.{Gen, Shrink}
import org.scalatest._
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.transaction._

class BlockStorageImplBlockOnlyTest extends PropSpec with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndPayments: Gen[(GenesisTransaction, PaymentTransaction, PaymentTransaction)] = for {
    master <- accountGen
    recipient <- otherAccountGen(candidate = master)
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    payment: PaymentTransaction <- paymentGeneratorP(master, recipient)
    payment2: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (genesis, payment, payment2)

  property("can apply valid blocks") {
    forAll(preconditionsAndPayments) { case ((genesis, payment, _)) =>
      val fp = setup()
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
      blocks.foreach(block => fp.blockchainUpdater.processBlock(block).explicitGet())
    }
  }

  property("can apply, rollback and reprocess valid blocks") {
    forAll(preconditionsAndPayments) { case ((genesis, payment, payment2)) =>
      val fp = setup()
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment), Seq(payment2)))
      fp.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
      fp.history.height() shouldBe 1
      fp.stateReader.height shouldBe 1
      fp.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      fp.history.height() shouldBe 2
      fp.stateReader.height shouldBe 2
      fp.blockchainUpdater.removeAfter(blocks.head.uniqueId) shouldBe true
      fp.history.height() shouldBe 1
      fp.stateReader.height shouldBe 1
      fp.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      fp.blockchainUpdater.processBlock(blocks(2)) shouldBe 'right
    }
  }

  property("can't apply block with invalid signature") {
    forAll(preconditionsAndPayments) { case ((genesis, payment, _)) =>
      val fp = setup()
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
      fp.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
      fp.blockchainUpdater.processBlock(malformSignature(blocks.last)) should produce("InvalidSignature")
    }
  }

  property("can't apply block with invalid signature after rollback") {
    forAll(preconditionsAndPayments) { case ((genesis, payment, _)) =>
      val fp = setup()
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
      fp.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
      fp.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      fp.blockchainUpdater.removeAfter(blocks.head.uniqueId) shouldBe true
      fp.blockchainUpdater.processBlock(malformSignature(blocks(1))) should produce("InvalidSignature")
    }
  }
}
