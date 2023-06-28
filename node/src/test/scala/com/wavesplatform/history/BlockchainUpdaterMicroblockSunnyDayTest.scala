package com.wavesplatform.history

import com.wavesplatform.account.{Address, AddressOrAlias, KeyPair}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto._
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.state.diffs._
import com.wavesplatform.test._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.transfer._
import org.scalacheck.Gen

class BlockchainUpdaterMicroblockSunnyDayTest extends PropSpec with DomainScenarioDrivenPropertyCheck {

  type Setup = (GenesisTransaction, TransferTransaction, TransferTransaction, TransferTransaction)
  val preconditionsAndPayments: Gen[Setup] = for {
    master <- accountGen
    alice  <- accountGen
    bob    <- accountGen
    ts     <- positiveIntGen
    fee    <- smallFeeGen
    genesis: GenesisTransaction = GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()
    masterToAlice: TransferTransaction <- wavesTransferGeneratorP(ts, master, alice.toAddress)
    aliceToBob  = createWavesTransfer(alice, bob.toAddress, masterToAlice.amount.value - fee - 1, fee, ts).explicitGet()
    aliceToBob2 = createWavesTransfer(alice, bob.toAddress, masterToAlice.amount.value - fee - 1, fee, ts + 1).explicitGet()
  } yield (genesis, masterToAlice, aliceToBob, aliceToBob2)

  property("all txs in different blocks: B0 <- B1 <- B2 <- B3!") {
    assume(BlockchainFeatures.implemented.contains(BlockchainFeatures.SmartAccounts.id))
    scenario(preconditionsAndPayments) { case (domain, (genesis, masterToAlice, aliceToBob, aliceToBob2)) =>
      val blocks = chainBlocks(Seq(Seq(genesis), Seq(masterToAlice), Seq(aliceToBob), Seq(aliceToBob2)))
      blocks.init.foreach(block => domain.blockchainUpdater.processBlock(block) should beRight)
      domain.blockchainUpdater.processBlock(blocks.last) should produce("unavailable funds")

      effBalance(genesis.recipient, domain) > 0 shouldBe true
      effBalance(masterToAlice.recipient, domain) shouldBe 0L
      effBalance(aliceToBob.recipient, domain) shouldBe 0L
    }
  }

  property("all txs in one block: B0 <- B0m1 <- B0m2 <- B0m3!") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, masterToAlice, aliceToBob, aliceToBob2)) =>
      val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, Seq(masterToAlice, aliceToBob, aliceToBob2).map(Seq(_)))
      domain.blockchainUpdater.processBlock(block) should beRight
      domain.blockchainUpdater.processMicroBlock(microBlocks(0)) should beRight
      domain.blockchainUpdater.processMicroBlock(microBlocks(1)) should beRight
      domain.blockchainUpdater.processMicroBlock(microBlocks(2)) should produce("unavailable funds")

      effBalance(genesis.recipient, domain) > 0 shouldBe true
      effBalance(masterToAlice.recipient, domain) > 0 shouldBe true
      effBalance(aliceToBob.recipient, domain) > 0 shouldBe true
    }
  }

  property("block references microBlock: B0 <- B1 <- B1m1 <- B2!") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, masterToAlice, aliceToBob, aliceToBob2)) =>
      val (block, microBlocks) = chainBaseAndMicro(randomSig, genesis, Seq(masterToAlice, aliceToBob, aliceToBob2).map(Seq(_)))
      domain.blockchainUpdater.processBlock(block) should beRight
      domain.blockchainUpdater.processMicroBlock(microBlocks(0)) should beRight
      domain.blockchainUpdater.processMicroBlock(microBlocks(1)) should beRight
      domain.blockchainUpdater.processMicroBlock(microBlocks(2)) should produce("unavailable funds")

      effBalance(genesis.recipient, domain) should be > 0L
      effBalance(masterToAlice.recipient, domain) should be > 0L
      effBalance(aliceToBob.recipient, domain) should be > 0L
    }
  }

  property("discards some of microBlocks: B0 <- B0m1 <- B0m2; B0m1 <- B1") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, masterToAlice, aliceToBob, aliceToBob2)) =>
      val (block0, microBlocks0) = chainBaseAndMicro(randomSig, genesis, Seq(masterToAlice, aliceToBob).map(Seq(_)))
      val block1                 = buildBlockOfTxs(microBlocks0.head.totalResBlockSig, Seq(aliceToBob2))
      domain.blockchainUpdater.processBlock(block0) should beRight
      domain.blockchainUpdater.processMicroBlock(microBlocks0(0)) should beRight
      domain.blockchainUpdater.processMicroBlock(microBlocks0(1)) should beRight
      domain.blockchainUpdater.processBlock(block1) should beRight

      effBalance(genesis.recipient, domain) > 0 shouldBe true
      effBalance(masterToAlice.recipient, domain) > 0 shouldBe true
      effBalance(aliceToBob.recipient, domain) shouldBe aliceToBob.amount.value
    }
  }

  property("discards all microBlocks: B0 <- B1 <- B1m1; B1 <- B2") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, masterToAlice, aliceToBob, aliceToBob2)) =>
      val block0                 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microBlocks1) = chainBaseAndMicro(block0.id(), masterToAlice, Seq(Seq(aliceToBob)))
      val block2                 = buildBlockOfTxs(block1.id(), Seq(aliceToBob2))
      domain.blockchainUpdater.processBlock(block0) should beRight
      domain.blockchainUpdater.processBlock(block1) should beRight
      domain.blockchainUpdater.processMicroBlock(microBlocks1.head) should beRight
      domain.blockchainUpdater.processBlock(block2) should beRight

      effBalance(genesis.recipient, domain) > 0 shouldBe true
      effBalance(masterToAlice.recipient, domain) shouldBe 0
      effBalance(aliceToBob.recipient, domain) shouldBe 0
    }
  }

  property("doesn't discard liquid block if competitor is not better: B0 <- B1 <- B1m1; B0 <- B2!") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, masterToAlice, aliceToBob, aliceToBob2)) =>
      val block0                 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microBlocks1) = chainBaseAndMicro(block0.id(), masterToAlice, Seq(Seq(aliceToBob)))
      val block2                 = buildBlockOfTxs(block0.id(), Seq(aliceToBob2), masterToAlice.timestamp)
      domain.blockchainUpdater.processBlock(block0) should beRight
      domain.blockchainUpdater.processBlock(block1) should beRight
      domain.blockchainUpdater.processMicroBlock(microBlocks1(0)) should beRight
      domain.blockchainUpdater.processBlock(block2) should beRight // silently discards worse version

      effBalance(genesis.recipient, domain) > 0 shouldBe true
      effBalance(masterToAlice.recipient, domain) shouldBe 1
      effBalance(aliceToBob.recipient, domain) shouldBe aliceToBob.amount.value
    }
  }

  property("discards liquid block if competitor is better: B0 <- B1 <- B1m1; B0 <- B2") {
    scenario(preconditionsAndPayments, MicroblocksActivatedAt0WavesSettings) { case (domain, (genesis, masterToAlice, aliceToBob, aliceToBob2)) =>
      val block0                 = buildBlockOfTxs(randomSig, Seq(genesis))
      val (block1, microBlocks1) = chainBaseAndMicro(block0.id(), masterToAlice, Seq(Seq(aliceToBob)))
      val otherSigner            = KeyPair(ByteStr(Array.fill(KeyLength)(1: Byte)))
      val block2 =
        customBuildBlockOfTxs(block0.id(), Seq(masterToAlice, aliceToBob2), otherSigner, 1, block1.header.timestamp - 1)
      domain.blockchainUpdater.processBlock(block0) should beRight
      domain.blockchainUpdater.processBlock(block1) should beRight
      domain.blockchainUpdater.processMicroBlock(microBlocks1(0)) should beRight
      domain.blockchainUpdater.processBlock(block2) should beRight

      effBalance(genesis.recipient, domain) > 0 shouldBe true
      effBalance(masterToAlice.recipient, domain) shouldBe 1
      effBalance(aliceToBob.recipient, domain) shouldBe aliceToBob.amount.value
    }
  }

  property("discarding some of microBlocks doesn't affect resulting state") {
    forAll(preconditionsAndPayments, accountGen) { case ((genesis, masterToAlice, aliceToBob, aliceToBob2), miner) =>
      val ts = genesis.timestamp

      val minerABalance = withDomain(MicroblocksActivatedAt0WavesSettings) { da =>
        val block0a                  = customBuildBlockOfTxs(randomSig, Seq(genesis), miner, 3: Byte, ts)
        val (block1a, microBlocks1a) = chainBaseAndMicro(block0a.id(), Seq(masterToAlice), Seq(Seq(aliceToBob)), miner, 3: Byte, ts)
        val block2a                  = customBuildBlockOfTxs(block1a.id(), Seq(aliceToBob2), miner, 3: Byte, ts)
        val block3a                  = customBuildBlockOfTxs(block2a.id(), Seq.empty, miner, 3: Byte, ts)
        da.blockchainUpdater.processBlock(block0a) should beRight
        da.blockchainUpdater.processBlock(block1a) should beRight
        da.blockchainUpdater.processMicroBlock(microBlocks1a(0)) should beRight
        da.blockchainUpdater.processBlock(block2a) should beRight
        da.blockchainUpdater.processBlock(block3a) should beRight

        da.balance(miner.toAddress)
      }

      val minerBBalance = withDomain(MicroblocksActivatedAt0WavesSettings) { db =>
        val block0b = customBuildBlockOfTxs(randomSig, Seq(genesis), miner, 3: Byte, ts)
        val block1b = customBuildBlockOfTxs(block0b.id(), Seq(masterToAlice), miner, 3: Byte, ts)
        val block2b = customBuildBlockOfTxs(block1b.id(), Seq(aliceToBob2), miner, 3: Byte, ts)
        val block3b = customBuildBlockOfTxs(block2b.id(), Seq.empty, miner, 3: Byte, ts)
        db.blockchainUpdater.processBlock(block0b) should beRight
        db.blockchainUpdater.processBlock(block1b) should beRight
        db.blockchainUpdater.processBlock(block2b) should beRight
        db.blockchainUpdater.processBlock(block3b) should beRight

        db.balance(miner.toAddress)
      }

      minerABalance shouldBe minerBBalance
    }
  }

  private def effBalance(aa: AddressOrAlias, domain: Domain): Long = aa match {
    case address: Address => domain.effBalance(address)
    case _                => fail("Unexpected address object")
  }
}
