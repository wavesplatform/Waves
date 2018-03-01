package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2.diffs._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction._
import scorex.transaction.assets.TransferTransaction

class BlockchainUpdaterBlockOnlyTest extends PropSpec
  with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {


  def preconditionsAndPayments(paymentsAmt: Int): Gen[(GenesisTransaction, Seq[TransferTransaction])] = for {
    master <- accountGen
    recipient <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    payments <- Gen.listOfN(paymentsAmt, wavesTransferGeneratorP(master, recipient))
  } yield (genesis, payments)

  property("can apply valid blocks") {
    scenario(preconditionsAndPayments(1)) { case (domain, (genesis, payments)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payments.head)))
      all(blocks.map(block => domain.blockchainUpdater.processBlock(block))) shouldBe 'right
    }
  }

  property("can apply, rollback and reprocess valid blocks") {
    scenario(preconditionsAndPayments(2)) { case (domain, (genesis, payments)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payments(0)), Seq(payments(1))))
      domain.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
      domain.history.height shouldBe 1
      domain.stateReader.height shouldBe 1
      domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      domain.history.height shouldBe 2
      domain.stateReader.height shouldBe 2
      domain.blockchainUpdater.removeAfter(blocks.head.uniqueId) shouldBe 'right
      domain.history.height shouldBe 1
      domain.stateReader.height shouldBe 1
      domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      domain.blockchainUpdater.processBlock(blocks(2)) shouldBe 'right
    }
  }

  property("can't apply block with invalid signature") {
    scenario(preconditionsAndPayments(1)) { case (domain, (genesis, payment)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), payment))
      domain.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
      domain.blockchainUpdater.processBlock(spoilSignature(blocks.last)) should produce("InvalidSignature")
    }
  }

  property("can't apply block with invalid signature after rollback") {
    scenario(preconditionsAndPayments(1)) { case (domain, (genesis, payment)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), payment))
      domain.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
      domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      domain.blockchainUpdater.removeAfter(blocks.head.uniqueId) shouldBe 'right
      domain.blockchainUpdater.processBlock(spoilSignature(blocks(1))) should produce("InvalidSignature")
    }
  }

  property("can process 11 blocks and then rollback to genesis") {
    scenario(preconditionsAndPayments(10)) { case (domain, (genesis, payments)) =>
      val blocks = chainBlocks(Seq(genesis) +: payments.map(Seq(_)))
      blocks.foreach { b =>
        domain.blockchainUpdater.processBlock(b) shouldBe 'right
      }
      domain.blockchainUpdater.removeAfter(blocks.head.uniqueId) shouldBe 'right
    }
  }
}
