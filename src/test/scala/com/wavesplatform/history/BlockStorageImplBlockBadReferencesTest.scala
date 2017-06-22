package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.state2._
import com.wavesplatform.state2.diffs._
import org.scalacheck.{Gen, Shrink}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import scorex.transaction.{GenesisTransaction, PaymentTransaction}

class BlockStorageImplBlockBadReferencesTest extends PropSpec with PropertyChecks with
  DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {
  private implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)

  val preconditionsAndPayments: Gen[(GenesisTransaction, PaymentTransaction, PaymentTransaction, PaymentTransaction)] = for {
    master <- accountGen
    recipient <- otherAccountGen(candidate = master)
    ts <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).right.get
    payment: PaymentTransaction <- paymentGeneratorP(master, recipient)
    payment2: PaymentTransaction <- paymentGeneratorP(master, recipient)
    payment3: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (genesis, payment, payment2, payment3)


  property("microBlock: referenced (micro)block doesn't exist") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2, payment3)) =>
      val block0 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microblocks1) = chainBaseAndMicro(block0.uniqueId, payment, Seq(payment2, payment3))
      val goodMicro = microblocks1(0)
      val badMicroRef = microblocks1(1).copy(prevResBlockSig = randomSig)

      domain.blockchainUpdater.processBlock(block0).explicitGet()
      domain.blockchainUpdater.processBlock(block1).explicitGet()
      domain.blockchainUpdater.processMicroBlock(goodMicro).explicitGet()
      domain.blockchainUpdater.processMicroBlock(badMicroRef) should produce("doesn't reference last known microBlock")
    }
  }

  property("microblock: first micro doesn't reference base block(references nothing)") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2, payment3)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
      val block0 = blocks(0)
      val block1 = blocks(1)
      val badMicroRef = buildMicroBlockOfTxs(block0.uniqueId, block1, Seq(payment2))._2
        .copy(prevResBlockSig = randomSig)
      domain.blockchainUpdater.processBlock(block0).explicitGet()
      domain.blockchainUpdater.processBlock(block1).explicitGet()
      domain.blockchainUpdater.processMicroBlock(badMicroRef) should produce("doesn't reference base block")
    }
  }

  property("microblock: first micro doesn't reference base block(references firm block)") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2, payment3)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
      val block0 = blocks(0)
      val block1 = blocks(1)
      val badMicroRef = buildMicroBlockOfTxs(block0.uniqueId, block1, Seq(payment2))._2
        .copy(prevResBlockSig = randomSig)
      domain.blockchainUpdater.processBlock(block0).explicitGet()
      domain.blockchainUpdater.processBlock(block1).explicitGet()
      domain.blockchainUpdater.processMicroBlock(badMicroRef) should produce("doesn't reference base block")
    }
  }

  property("microblock: no base block at all") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2, payment3)) =>
      val block0 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microblocks1) = chainBaseAndMicro(block0.uniqueId, payment, Seq(payment2))
      domain.blockchainUpdater.processBlock(block0).explicitGet()
      domain.blockchainUpdater.processBlock(block1).explicitGet()
      domain.blockchainUpdater.removeAfter(block0.uniqueId) shouldBe true
      domain.blockchainUpdater.processMicroBlock(microblocks1.head) should produce("No base block exists")
    }
  }

  property("microblock: follow-up micro doesn't reference last known micro") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2, payment3)) =>
      val block0 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microblocks1) = chainBaseAndMicro(block0.uniqueId, payment, Seq(payment2, payment3))
      val goodMicro = microblocks1(0)
      val badRefMicro = microblocks1(1).copy(prevResBlockSig = block1.uniqueId)
      domain.blockchainUpdater.processBlock(block0).explicitGet()
      domain.blockchainUpdater.processBlock(block1).explicitGet()
      domain.blockchainUpdater.processMicroBlock(goodMicro).explicitGet()
      domain.blockchainUpdater.processMicroBlock(badRefMicro) should produce("doesn't reference last known microBlock")
    }
  }

  property("block: second 'genesis' block") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2, payment3)) =>
      val block0 = buildBlockOfTxs(randomSig, Seq(genesis, payment))
      val block1 = buildBlockOfTxs(randomSig, Seq(genesis, payment2))
      domain.blockchainUpdater.processBlock(block0) shouldBe 'right
      domain.blockchainUpdater.processBlock(block1) should produce("References non-existing block")
    }
  }

  property("block: incorrect or non-existing block when liquid is empty") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2, payment3)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment), Seq(payment2)))
      domain.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
      domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      domain.blockchainUpdater.removeAfter(blocks.head.uniqueId) shouldBe true
      val block2 = buildBlockOfTxs(randomSig, Seq(payment3))
      domain.blockchainUpdater.processBlock(block2) should produce("References incorrect or non-existing block")
    }
  }

  property("block: overwrite liquid block") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2, payment3)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment), Seq(payment2)))
      val block2v2 = buildBlockOfTxs(blocks(1).uniqueId, Seq(payment3))
      domain.blockchainUpdater.processBlock(blocks(0)) shouldBe 'right
      domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      domain.blockchainUpdater.processBlock(blocks(2)) shouldBe 'right
      domain.blockchainUpdater.processBlock(block2v2) shouldBe 'right
    }
  }

  property("block: incorrect or non-existing block when liquid exists") {
    scenario(preconditionsAndPayments) { case (domain, (genesis, payment, payment2, payment3)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment), Seq(payment2)))
      val block1v2 = buildBlockOfTxs(blocks(0).uniqueId, Seq(payment3))
      domain.blockchainUpdater.processBlock(blocks(0)) shouldBe 'right
      domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
      domain.blockchainUpdater.processBlock(blocks(2)) shouldBe 'right
      domain.blockchainUpdater.processBlock(block1v2) should produce("References incorrect or non-existing block")
    }
  }
}
