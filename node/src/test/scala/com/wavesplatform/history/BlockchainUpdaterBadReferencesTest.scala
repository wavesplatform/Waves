package com.wavesplatform.history

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.state.diffs.*
import com.wavesplatform.test.*
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.transfer.*
import org.scalacheck.Gen

class BlockchainUpdaterBadReferencesTest extends PropSpec with DomainScenarioDrivenPropertyCheck {

  val preconditionsAndPayments: Gen[(GenesisTransaction, TransferTransaction, TransferTransaction, TransferTransaction)] = for {
    master    <- accountGen
    recipient <- accountGen
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
    payment: TransferTransaction  <- wavesTransferGeneratorP(ts, master, recipient.toAddress)
    payment2: TransferTransaction <- wavesTransferGeneratorP(ts, master, recipient.toAddress)
    payment3: TransferTransaction <- wavesTransferGeneratorP(ts, master, recipient.toAddress)
  } yield (genesis, payment, payment2, payment3)

  property("microBlock: referenced (micro)block doesn't exist") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, payment, payment2, payment3)) =>
      val block0                 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microblocks1) = chainBaseAndMicro(block0.id(), payment, Seq(payment2, payment3).map(Seq(_)))
      val goodMicro              = microblocks1(0)
      val badMicroRef            = microblocks1(1).copy(reference = randomSig)

      domain.blockchainUpdater.processBlock(block0) should beRight
      domain.blockchainUpdater.processBlock(block1) should beRight
      domain.blockchainUpdater.processMicroBlock(goodMicro, None) should beRight
      domain.blockchainUpdater.processMicroBlock(badMicroRef, None) should produce("doesn't reference last known microBlock")
    }
  }

  property("microblock: first micro doesn't reference base block(references nothing)") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, payment, payment2, payment3)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
      val block0 = blocks(0)
      val block1 = blocks(1)
      val badMicroRef = buildMicroBlockOfTxs(block0.id(), block1, Seq(payment2), defaultSigner)._2
        .copy(reference = randomSig)
      domain.blockchainUpdater.processBlock(block0) should beRight
      domain.blockchainUpdater.processBlock(block1) should beRight
      domain.blockchainUpdater.processMicroBlock(badMicroRef, None) should produce("doesn't reference base block")
    }
  }

  property("microblock: first micro doesn't reference base block(references firm block)") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, payment, payment2, _)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment)))
      val block0 = blocks(0)
      val block1 = blocks(1)
      val badMicroRef = buildMicroBlockOfTxs(block0.id(), block1, Seq(payment2), defaultSigner)._2
        .copy(reference = randomSig)
      domain.blockchainUpdater.processBlock(block0) should beRight
      domain.blockchainUpdater.processBlock(block1) should beRight
      domain.blockchainUpdater.processMicroBlock(badMicroRef, None) should produce("doesn't reference base block")
    }
  }

  property("microblock: no base block at all") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, payment, payment2, _)) =>
      val block0                 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microblocks1) = chainBaseAndMicro(block0.id(), payment, Seq(payment2).map(Seq(_)))
      domain.blockchainUpdater.processBlock(block0) should beRight
      domain.blockchainUpdater.processBlock(block1) should beRight
      domain.blockchainUpdater.removeAfter(block0.id()) should beRight
      domain.blockchainUpdater.processMicroBlock(microblocks1.head, None) should produce("No base block exists")
    }
  }

  property("microblock: follow-up micro doesn't reference last known micro") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, payment, payment2, payment3)) =>
      val block0                 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microblocks1) = chainBaseAndMicro(block0.id(), payment, Seq(payment2, payment3).map(Seq(_)))
      val goodMicro              = microblocks1(0)
      val badRefMicro            = microblocks1(1).copy(reference = block1.id())
      domain.blockchainUpdater.processBlock(block0) should beRight
      domain.blockchainUpdater.processBlock(block1) should beRight
      domain.blockchainUpdater.processMicroBlock(goodMicro, None) should beRight
      domain.blockchainUpdater.processMicroBlock(badRefMicro, None) should produce("doesn't reference last known microBlock")
    }
  }

  property("block: second 'genesis' block") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, payment, payment2, payment3)) =>
      val block0 = buildBlockOfTxs(randomSig, Seq(genesis, payment))
      val block1 = buildBlockOfTxs(randomSig, Seq(genesis, payment2))
      domain.blockchainUpdater.processBlock(block0) should beRight
      domain.blockchainUpdater.processBlock(block1) should produce("References incorrect or non-existing block")
    }
  }

  property("block: incorrect or non-existing block when liquid is empty") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, payment, payment2, payment3)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(payment), Seq(payment2)))
      domain.blockchainUpdater.processBlock(blocks.head) should beRight
      domain.blockchainUpdater.processBlock(blocks(1)) should beRight
      domain.blockchainUpdater.removeAfter(blocks.head.id()) should beRight
      val block2 = buildBlockOfTxs(randomSig, Seq(payment3))
      domain.blockchainUpdater.processBlock(block2) should produce("References incorrect or non-existing block")
    }
  }

  property("block: incorrect or non-existing block when liquid exists") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, payment, payment2, payment3)) =>
      val blocks   = chainBlocks(Seq(Seq(genesis), Seq(payment), Seq(payment2)))
      val block1v2 = buildBlockOfTxs(blocks(0).id(), Seq(payment3))
      domain.blockchainUpdater.processBlock(blocks(0)) should beRight
      domain.blockchainUpdater.processBlock(blocks(1)) should beRight
      domain.blockchainUpdater.processBlock(blocks(2)) should beRight
      domain.blockchainUpdater.processBlock(block1v2) should produce("References incorrect or non-existing block")
    }
  }
}
