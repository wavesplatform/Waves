package com.wavesplatform.history

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.state.diffs._
import com.wavesplatform.test._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.transfer._
import org.scalacheck.Gen

class BlockchainUpdaterBlockOnlyTest extends PropSpec with DomainScenarioDrivenPropertyCheck {

  def preconditionsAndPayments(paymentsAmt: Int): Gen[(GenesisTransaction, Seq[TransferTransaction])] =
    for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- positiveIntGen
      genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
      payments <- Gen.listOfN(paymentsAmt, wavesTransferGeneratorP(ts, master, recipient.toAddress))
    } yield (genesis, payments)

  property("can apply valid blocks") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments(1)) { case (domain, (genesis, payments)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payments.head)))
      blocks.map(block => domain.blockchainUpdater.processBlock(block) should beRight)
    }
  }

  property("can apply, rollback and reprocess valid blocks") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments(2)) { case (domain, (genesis, payments)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payments.head), Seq(payments(1))))
      domain.blockchainUpdater.processBlock(blocks.head) should beRight
      domain.blockchainUpdater.height shouldBe 1
      domain.blockchainUpdater.processBlock(blocks(1)) should beRight
      domain.blockchainUpdater.height shouldBe 2
      domain.blockchainUpdater.removeAfter(blocks.head.id()) should beRight
      domain.blockchainUpdater.height shouldBe 1
      domain.blockchainUpdater.processBlock(blocks(1)) should beRight
      domain.blockchainUpdater.processBlock(blocks(2)) should beRight
    }
  }

  property("can't apply block with invalid signature") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments(1)) { case (domain, (genesis, payment)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), payment))
      domain.blockchainUpdater.processBlock(blocks.head) should beRight
      domain.blockchainUpdater.processBlock(spoilSignature(blocks.last)) should produce("invalid signature")
    }
  }

  property("can't apply block with invalid signature after rollback") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments(1)) { case (domain, (genesis, payment)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), payment))
      domain.blockchainUpdater.processBlock(blocks.head) should beRight
      domain.blockchainUpdater.processBlock(blocks(1)) should beRight
      domain.blockchainUpdater.removeAfter(blocks.head.id()) should beRight
      domain.blockchainUpdater.processBlock(spoilSignature(blocks(1))) should produce("invalid signature")
    }
  }

  property("can process 11 blocks and then rollback to genesis") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments(10)) { case (domain, (genesis, payments)) =>
      val blocks = chainBlocks(Seq(genesis) +: payments.map(Seq(_)))
      blocks.foreach { b =>
        domain.blockchainUpdater.processBlock(b) should beRight
      }
      domain.blockchainUpdater.removeAfter(blocks.head.id()) should beRight
    }
  }
}
