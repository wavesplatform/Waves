package com.wavesplatform.history

import com.wavesplatform.TransactionGen
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.diffs._
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.transfer._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class BlockchainUpdaterBadReferencesTest
    extends PropSpec
    with PropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with TransactionGen {

  val preconditionsAndPayments: Gen[(GenesisTransaction, TransferTransactionV1, TransferTransactionV1, TransferTransactionV1)] = for {
    master    <- accountGen
    recipient <- accountGen
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    payment: TransferTransactionV1  <- wavesTransferGeneratorP(ts, master, recipient)
    payment2: TransferTransactionV1 <- wavesTransferGeneratorP(ts, master, recipient)
    payment3: TransferTransactionV1 <- wavesTransferGeneratorP(ts, master, recipient)
  } yield (genesis, payment, payment2, payment3)

  property("microBlock: referenced (micro)block doesn't exist") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) {
      case (domain, (genesis, payment, payment2, payment3)) =>
        val block0                 = buildBlockOfTxs(randomSig, Seq(genesis))
        val (block1, microblocks1) = chainBaseAndMicro(block0.uniqueId, payment, Seq(payment2, payment3).map(Seq(_)))
        val goodMicro              = microblocks1(0)
        val badMicroRef            = microblocks1(1).copy(prevResBlockSig = randomSig)

        domain.blockchainUpdater.processBlock(block0).explicitGet()
        domain.blockchainUpdater.processBlock(block1).explicitGet()
        domain.blockchainUpdater.processMicroBlock(goodMicro).explicitGet()
        domain.blockchainUpdater.processMicroBlock(badMicroRef) should produce("doesn't reference last known microBlock")
    }
  }

  property("microblock: first micro doesn't reference base block(references nothing)") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) {
      case (domain, (genesis, payment, payment2, payment3)) =>
        val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
        val block0 = blocks(0)
        val block1 = blocks(1)
        val badMicroRef = buildMicroBlockOfTxs(block0.uniqueId, block1, Seq(payment2), defaultSigner)._2
          .copy(prevResBlockSig = randomSig)
        domain.blockchainUpdater.processBlock(block0).explicitGet()
        domain.blockchainUpdater.processBlock(block1).explicitGet()
        domain.blockchainUpdater.processMicroBlock(badMicroRef) should produce("doesn't reference base block")
    }
  }

  property("microblock: first micro doesn't reference base block(references firm block)") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) {
      case (domain, (genesis, payment, payment2, payment3)) =>
        val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
        val block0 = blocks(0)
        val block1 = blocks(1)
        val badMicroRef = buildMicroBlockOfTxs(block0.uniqueId, block1, Seq(payment2), defaultSigner)._2
          .copy(prevResBlockSig = randomSig)
        domain.blockchainUpdater.processBlock(block0).explicitGet()
        domain.blockchainUpdater.processBlock(block1).explicitGet()
        domain.blockchainUpdater.processMicroBlock(badMicroRef) should produce("doesn't reference base block")
    }
  }

  property("microblock: no base block at all") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) {
      case (domain, (genesis, payment, payment2, payment3)) =>
        val block0                 = buildBlockOfTxs(randomSig, Seq(genesis))
        val (block1, microblocks1) = chainBaseAndMicro(block0.uniqueId, payment, Seq(payment2).map(Seq(_)))
        domain.blockchainUpdater.processBlock(block0).explicitGet()
        domain.blockchainUpdater.processBlock(block1).explicitGet()
        domain.blockchainUpdater.removeAfter(block0.uniqueId).explicitGet()
        domain.blockchainUpdater.processMicroBlock(microblocks1.head) should produce("No base block exists")
    }
  }

  property("microblock: follow-up micro doesn't reference last known micro") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) {
      case (domain, (genesis, payment, payment2, payment3)) =>
        val block0                 = buildBlockOfTxs(randomSig, Seq(genesis))
        val (block1, microblocks1) = chainBaseAndMicro(block0.uniqueId, payment, Seq(payment2, payment3).map(Seq(_)))
        val goodMicro              = microblocks1(0)
        val badRefMicro            = microblocks1(1).copy(prevResBlockSig = block1.uniqueId)
        domain.blockchainUpdater.processBlock(block0).explicitGet()
        domain.blockchainUpdater.processBlock(block1).explicitGet()
        domain.blockchainUpdater.processMicroBlock(goodMicro).explicitGet()
        domain.blockchainUpdater.processMicroBlock(badRefMicro) should produce("doesn't reference last known microBlock")
    }
  }

  property("block: second 'genesis' block") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) {
      case (domain, (genesis, payment, payment2, payment3)) =>
        val block0 = buildBlockOfTxs(randomSig, Seq(genesis, payment))
        val block1 = buildBlockOfTxs(randomSig, Seq(genesis, payment2))
        domain.blockchainUpdater.processBlock(block0) shouldBe 'right
        domain.blockchainUpdater.processBlock(block1) should produce("References incorrect or non-existing block")
    }
  }

  property("block: incorrect or non-existing block when liquid is empty") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) {
      case (domain, (genesis, payment, payment2, payment3)) =>
        val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment), Seq(payment2)))
        domain.blockchainUpdater.processBlock(blocks.head) shouldBe 'right
        domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
        domain.blockchainUpdater.removeAfter(blocks.head.uniqueId).explicitGet()
        val block2 = buildBlockOfTxs(randomSig, Seq(payment3))
        domain.blockchainUpdater.processBlock(block2) should produce("References incorrect or non-existing block")
    }
  }

  property("block: incorrect or non-existing block when liquid exists") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) {
      case (domain, (genesis, payment, payment2, payment3)) =>
        val blocks   = chainBlocks(Seq(Seq(genesis), Seq(payment), Seq(payment2)))
        val block1v2 = buildBlockOfTxs(blocks(0).uniqueId, Seq(payment3))
        domain.blockchainUpdater.processBlock(blocks(0)) shouldBe 'right
        domain.blockchainUpdater.processBlock(blocks(1)) shouldBe 'right
        domain.blockchainUpdater.processBlock(blocks(2)) shouldBe 'right
        domain.blockchainUpdater.processBlock(block1v2) should produce("References incorrect or non-existing block")
    }
  }
}
