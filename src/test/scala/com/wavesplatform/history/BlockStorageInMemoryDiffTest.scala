package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import org.scalacheck.{Gen, Shrink}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction._

class BlockStorageInMemoryDiffTest extends PropSpec with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {

  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndPayments: Gen[(GenesisTransaction, PaymentTransaction, PaymentTransaction)] = for {
    master <- accountGen
    recipient <- accountGen
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    payment: PaymentTransaction <- paymentGeneratorP(master, recipient)
    payment2: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (genesis, payment, payment2)

  property("compactification with liquid block doesn't affect state") {

    scenario(preconditionsAndPayments) { case (domain, (genesis, payment1, payment2)) =>

      val totalAmountOfBlocks = MinInMemoryDiffSize * 2 + 2
      val blocksWithoutCompactification = chainBlocks(Seq(genesis) +: Seq.fill(MinInMemoryDiffSize * 2 - 1)(Seq.empty[Transaction]) :+ Seq(payment1))
      val blockTriggersCompactification = buildBlockOfTxs(blocksWithoutCompactification.last.uniqueId, Seq(payment2))

      blocksWithoutCompactification.foreach(b => domain.blockchainUpdater.processBlock(b).explicitGet())
      val mastersBalanceAfterPayment1 = domain.stateReader.accountPortfolio(genesis.recipient).balance
      mastersBalanceAfterPayment1 shouldBe (ENOUGH_AMT - payment1.amount - payment1.fee)

      domain.blockchainUpdater.processBlock(blockTriggersCompactification).explicitGet()

      domain.history.height() shouldBe totalAmountOfBlocks
      domain.stateReader.height shouldBe totalAmountOfBlocks

      val mastersBalanceAfterPayment1AndPayment2 = domain.stateReader.accountPortfolio(genesis.recipient).balance
      mastersBalanceAfterPayment1AndPayment2 shouldBe (ENOUGH_AMT - payment1.amount - payment1.fee - payment2.amount - payment2.fee)
    }
  }
  property("compactification without liquid block doesn't affect state") {

  }
}
