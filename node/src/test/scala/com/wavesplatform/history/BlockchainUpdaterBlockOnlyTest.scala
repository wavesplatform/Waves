package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.transfer._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class BlockchainUpdaterBlockOnlyTest extends PropSpec with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {

  def preconditionsAndPayments(paymentsAmt: Int): Gen[(GenesisTransaction, Seq[TransferTransactionV1])] =
    for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      payments <- Gen.listOfN(paymentsAmt, wavesTransferGeneratorP(ts, master, recipient))
    } yield (genesis, payments)

  property("can apply valid blocks") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments(1)) {
      case (domain, (genesis, payments)) =>
        val blocks = chainBlocks(Seq(Seq(genesis), Seq(payments.head)))
        all(blocks.map(block => domain.blockchainUpdater.processBlock(block))) shouldBe 'right
    }
  }

  property("can apply, rollback and reprocess valid blocks") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments(2)) {
      case (domain, (genesis, payments)) =>
        val blocks = chainBlocks(Seq(Seq(genesis), Seq(payments(0)), Seq(payments(1))))
        domain.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
        domain.blockchainUpdater.height shouldBe 1
        domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
        domain.blockchainUpdater.height shouldBe 2
        domain.blockchainUpdater.removeAfter(blocks.head.uniqueId) shouldBe 'right
        domain.blockchainUpdater.height shouldBe 1
        domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
        domain.blockchainUpdater.processBlock(blocks(2)) shouldBe 'right
    }
  }

  property("can't apply block with invalid signature") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments(1)) {
      case (domain, (genesis, payment)) =>
        val blocks = chainBlocks(Seq(Seq(genesis), payment))
        domain.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
        domain.blockchainUpdater.processBlock(spoilSignature(blocks.last)) should produce("InvalidSignature")
    }
  }

  property("can't apply block with invalid signature after rollback") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments(1)) {
      case (domain, (genesis, payment)) =>
        val blocks = chainBlocks(Seq(Seq(genesis), payment))
        domain.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
        domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
        domain.blockchainUpdater.removeAfter(blocks.head.uniqueId) shouldBe 'right
        domain.blockchainUpdater.processBlock(spoilSignature(blocks(1))) should produce("InvalidSignature")
    }
  }

  property("can process 11 blocks and then rollback to genesis") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments(10)) {
      case (domain, (genesis, payments)) =>
        val blocks = chainBlocks(Seq(genesis) +: payments.map(Seq(_)))
        blocks.foreach { b =>
          domain.blockchainUpdater.processBlock(b) shouldBe 'right
        }
        domain.blockchainUpdater.removeAfter(blocks.head.uniqueId) shouldBe 'right
    }
  }
}
