package com.wavesplatform.history

import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import com.wavesplatform.{TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction._
import scorex.transaction.assets.TransferTransaction

class BlockchainUpdaterInMemoryDiffTest extends PropSpec
  with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen with WithDB {
  val preconditionsAndPayments: Gen[(GenesisTransaction, TransferTransaction, TransferTransaction)] = for {
    master <- accountGen
    recipient <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    payment: TransferTransaction <- wavesTransferGeneratorP(master, recipient)
    payment2: TransferTransaction <- wavesTransferGeneratorP(master, recipient)
  } yield (genesis, payment, payment2)

  property("compactification with liquid block doesn't make liquid block affect state once") {

    scenario(preconditionsAndPayments, db) { case (domain, (genesis, payment1, payment2)) =>
      val blocksWithoutCompactification = chainBlocks(
        Seq(genesis) +:
          Seq.fill(MaxTransactionsPerBlockDiff * 2 - 1)(Seq.empty[Transaction]) :+
          Seq(payment1))
      val blockTriggersCompactification = buildBlockOfTxs(blocksWithoutCompactification.last.uniqueId, Seq(payment2))

      blocksWithoutCompactification.foreach(b => domain.blockchainUpdater.processBlock(b).explicitGet())
      val mastersBalanceAfterPayment1 = domain.stateReader().accountPortfolio(genesis.recipient).balance
      mastersBalanceAfterPayment1 shouldBe (ENOUGH_AMT - payment1.amount - payment1.fee)

      domain.history.height() shouldBe MaxTransactionsPerBlockDiff * 2 + 1
      domain.stateReader().height shouldBe MaxTransactionsPerBlockDiff * 2 + 1

      domain.blockchainUpdater.processBlock(blockTriggersCompactification).explicitGet()

      domain.history.height() shouldBe MaxTransactionsPerBlockDiff * 2 + 2
      domain.stateReader().height shouldBe MaxTransactionsPerBlockDiff * 2 + 2

      val mastersBalanceAfterPayment1AndPayment2 = domain.stateReader().accountPortfolio(genesis.recipient).balance
      mastersBalanceAfterPayment1AndPayment2 shouldBe (ENOUGH_AMT - payment1.amount - payment1.fee - payment2.amount - payment2.fee)
    }
  }
  property("compactification without liquid block doesn't make liquid block affect state once") {
    scenario(preconditionsAndPayments, db) { case (domain, (genesis, payment1, payment2)) =>
      val firstBlocks = chainBlocks(Seq(Seq(genesis)) ++ Seq.fill(MaxTransactionsPerBlockDiff * 2 - 2)(Seq.empty[Transaction]))
      val payment1Block = buildBlockOfTxs(firstBlocks.last.uniqueId, Seq(payment1))
      val emptyBlock = buildBlockOfTxs(payment1Block.uniqueId, Seq.empty)
      val blockTriggersCompactification = buildBlockOfTxs(payment1Block.uniqueId, Seq(payment2))

      firstBlocks.foreach(b => domain.blockchainUpdater.processBlock(b).explicitGet())
      domain.blockchainUpdater.processBlock(payment1Block).explicitGet()
      domain.blockchainUpdater.processBlock(emptyBlock).explicitGet()
      val mastersBalanceAfterPayment1 = domain.stateReader().accountPortfolio(genesis.recipient).balance
      mastersBalanceAfterPayment1 shouldBe (ENOUGH_AMT - payment1.amount - payment1.fee)

      // discard liquid block
      domain.blockchainUpdater.removeAfter(payment1Block.uniqueId)
      domain.blockchainUpdater.processBlock(blockTriggersCompactification).explicitGet()

      domain.history.height() shouldBe MaxTransactionsPerBlockDiff * 2 + 1
      domain.stateReader().height shouldBe MaxTransactionsPerBlockDiff * 2 + 1

      val mastersBalanceAfterPayment1AndPayment2 = domain.stateReader().accountPortfolio(genesis.recipient).balance
      mastersBalanceAfterPayment1AndPayment2 shouldBe (ENOUGH_AMT - payment1.amount - payment1.fee - payment2.amount - payment2.fee)
    }
  }
}
