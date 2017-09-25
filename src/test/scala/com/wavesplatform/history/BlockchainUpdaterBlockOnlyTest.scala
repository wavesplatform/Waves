package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2.diffs._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction._

class BlockchainUpdaterBlockOnlyTest extends PropSpec with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {


  val preconditionsAndPayments: Gen[(GenesisTransaction, PaymentTransaction, PaymentTransaction)] = for {
    master <- accountGen
    recipient <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    payment: PaymentTransaction <- paymentGeneratorP(master, recipient)
    payment2: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (genesis, payment, payment2)

  property("can apply valid blocks") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, _)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
      all(blocks.map(block => domain.blockchainUpdater.processBlock(block))) shouldBe 'right
    }
  }

  property("can apply, rollback and reprocess valid blocks") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment), Seq(payment2)))
      domain.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
      domain.history.height() shouldBe 1
      domain.stateReader.height shouldBe 1
      domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      domain.history.height() shouldBe 2
      domain.stateReader.height shouldBe 2
      domain.blockchainUpdater.removeAfter(blocks.head.uniqueId) shouldBe 'right
      domain.history.height() shouldBe 1
      domain.stateReader.height shouldBe 1
      domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      domain.blockchainUpdater.processBlock(blocks(2)) shouldBe 'right
    }
  }

  property("can't apply block with invalid signature") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, _)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
      domain.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
      domain.blockchainUpdater.processBlock(spoilSignature(blocks.last)) should produce("InvalidSignature")
    }
  }

  property("can't apply block with invalid signature after rollback") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, _)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
      domain.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
      domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      domain.blockchainUpdater.removeAfter(blocks.head.uniqueId) shouldBe 'right
      domain.blockchainUpdater.processBlock(spoilSignature(blocks(1))) should produce("InvalidSignature")
    }
  }
}
