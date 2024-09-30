package com.wavesplatform.history

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.state.diffs.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.*
import com.wavesplatform.transaction.transfer.*
import org.scalacheck.Gen

class BlockchainUpdaterInMemoryDiffTest extends PropSpec with DomainScenarioDrivenPropertyCheck {
  val preconditionsAndPayments: Gen[(GenesisTransaction, TransferTransaction, TransferTransaction)] = for {
    master    <- accountGen
    recipient <- accountGen
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
    payment: TransferTransaction  <- wavesTransferGeneratorP(ts, master, recipient.toAddress)
    payment2: TransferTransaction <- wavesTransferGeneratorP(ts, master, recipient.toAddress)
  } yield (genesis, payment, payment2)

  property("compaction with liquid block doesn't make liquid block affect state once") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment1, payment2)) =>
      val blocksWithoutCompaction = chainBlocks(
        Seq(genesis) +:
          Seq.fill(MaxTransactionsPerBlockDiff * 2 - 1)(Seq.empty[Transaction]) :+
          Seq(payment1)
      )
      val blockTriggersCompaction = buildBlockOfTxs(blocksWithoutCompaction.last.id(), Seq(payment2))

      blocksWithoutCompaction.foreach(b => domain.blockchainUpdater.processBlock(b) should beRight)
      val mastersBalanceAfterPayment1 = domain.balance(genesis.recipient)
      mastersBalanceAfterPayment1 shouldBe (ENOUGH_AMT - payment1.amount.value - payment1.fee.value)

      domain.blockchainUpdater.height shouldBe MaxTransactionsPerBlockDiff * 2 + 1

      domain.blockchainUpdater.processBlock(blockTriggersCompaction) should beRight

      domain.blockchainUpdater.height shouldBe MaxTransactionsPerBlockDiff * 2 + 2

      val mastersBalanceAfterPayment1AndPayment2 = domain.blockchainUpdater.balance(genesis.recipient)
      mastersBalanceAfterPayment1AndPayment2 shouldBe (ENOUGH_AMT - payment1.amount.value - payment1.fee.value - payment2.amount.value - payment2.fee.value)
    }
  }
  property("compaction without liquid block doesn't make liquid block affect state once") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment1, payment2)) =>
      val firstBlocks             = chainBlocks(Seq(Seq(genesis)) ++ Seq.fill(MaxTransactionsPerBlockDiff * 2 - 2)(Seq.empty[Transaction]))
      val payment1Block           = buildBlockOfTxs(firstBlocks.last.id(), Seq(payment1))
      val emptyBlock              = buildBlockOfTxs(payment1Block.id(), Seq.empty)
      val blockTriggersCompaction = buildBlockOfTxs(payment1Block.id(), Seq(payment2))

      firstBlocks.foreach(b => domain.blockchainUpdater.processBlock(b) should beRight)
      domain.blockchainUpdater.processBlock(payment1Block) should beRight
      domain.blockchainUpdater.processBlock(emptyBlock) should beRight
      val mastersBalanceAfterPayment1 = domain.blockchainUpdater.balance(genesis.recipient)
      mastersBalanceAfterPayment1 shouldBe (ENOUGH_AMT - payment1.amount.value - payment1.fee.value)

      // discard liquid block
      domain.blockchainUpdater.removeAfter(payment1Block.id())
      domain.blockchainUpdater.processBlock(blockTriggersCompaction) should beRight

      domain.blockchainUpdater.height shouldBe MaxTransactionsPerBlockDiff * 2 + 1

      val mastersBalanceAfterPayment1AndPayment2 = domain.blockchainUpdater.balance(genesis.recipient)
      mastersBalanceAfterPayment1AndPayment2 shouldBe (ENOUGH_AMT - payment1.amount.value - payment1.fee.value - payment2.amount.value - payment2.fee.value)
    }
  }
}
