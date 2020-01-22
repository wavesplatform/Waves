package com.wavesplatform.history

import com.wavesplatform._
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.transaction.GenesisTransaction
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class BlockchainUpdaterKeyAndMicroBlockConflictTest
    extends PropSpec
    with PropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with TransactionGen
    with BlocksTransactionsHelpers {

  property("new key block should be validated to previous") {
    forAll(Preconditions.conflictingTransfers()) {
      case (prevBlock, keyBlock, microBlocks, keyBlock1) =>
        withDomain(MicroblocksActivatedAt0WavesSettings) { d =>
          d.blockchainUpdater.processBlock(prevBlock) shouldBe 'right
          d.blockchainUpdater.processBlock(keyBlock) shouldBe 'right

          microBlocks.foreach(d.blockchainUpdater.processMicroBlock(_) shouldBe 'right)

          d.blockchainUpdater.processBlock(keyBlock1) shouldBe 'right
        }
    }

    forAll(Preconditions.conflictingTransfersInMicro()) {
      case (prevBlock, keyBlock, microBlocks, keyBlock1) =>
        withDomain(MicroblocksActivatedAt0WavesSettings) { d =>
          d.blockchainUpdater.processBlock(prevBlock) shouldBe 'right
          d.blockchainUpdater.processBlock(keyBlock) shouldBe 'right

          microBlocks.foreach(d.blockchainUpdater.processMicroBlock(_) shouldBe 'right)

          d.blockchainUpdater.processBlock(keyBlock1) shouldBe 'right
        }
    }

    forAll(Preconditions.leaseAndLeaseCancel()) {
      case (genesisBlock, leaseBlock, keyBlock, microBlocks, transferBlock, secondAccount) =>
        withDomain(MicroblocksActivatedAt0WavesSettings) { d =>
          Seq(genesisBlock, leaseBlock, keyBlock).foreach(d.blockchainUpdater.processBlock(_) shouldBe 'right)
          assert(d.blockchainUpdater.effectiveBalance(secondAccount, 0) > 0)

          microBlocks.foreach(d.blockchainUpdater.processMicroBlock(_) shouldBe 'right)
          assert(d.blockchainUpdater.effectiveBalance(secondAccount, 0, Some(leaseBlock.uniqueId)) > 0)

          assert(d.blockchainUpdater.processBlock(transferBlock).toString.contains("negative effective balance"))
        }
    }
  }

  property("data keys should not be duplicated") {
    forAll(Preconditions.duplicateDataKeys()) {
      case (genesisBlock, Seq(block1, block2), microBlocks, address) =>
        withDomain(DataAndMicroblocksActivatedAt0WavesSettings) { d =>
          Seq(genesisBlock, block1, block2).foreach(d.blockchainUpdater.processBlock(_) shouldBe 'right)
          d.blockchainUpdater.accountDataKeys(address) shouldBe Set("test")

          microBlocks.foreach(d.blockchainUpdater.processMicroBlock(_) shouldBe 'right)
          d.blockchainUpdater.accountDataKeys(address) shouldBe Set("test")
        }
    }
  }

  private[this] object Preconditions {
    import QuickTX._
    import UnsafeBlocks._

    def conflictingTransfers(): Gen[(Block, Block, Seq[MicroBlock], Block)] = {
      for {
        richAccount   <- accountGen
        secondAccount <- accountGen

        tsAmount = FeeAmount * 10

        blockTime = ntpNow
        transfer1 <- transfer(richAccount, secondAccount, tsAmount, validTimestampGen(blockTime))
        transfer2 <- transfer(secondAccount, richAccount, tsAmount - FeeAmount, validTimestampGen(blockTime))
        transfer3 <- transfer(secondAccount, richAccount, tsAmount - FeeAmount, validTimestampGen(blockTime))
      } yield {
        val genesisBlock = unsafeBlock(
          reference = randomSig,
          txs = Seq(GenesisTransaction.create(richAccount, tsAmount + FeeAmount, 0).explicitGet()),
          signer = TestBlock.defaultSigner,
          version = 3,
          timestamp = 0
        )

        val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
          totalRefTo = genesisBlock.signerData.signature,
          base = Seq(transfer1),
          micros = Seq(Seq(transfer2)),
          signer = richAccount,
          version = 3,
          timestamp = blockTime
        )

        val (keyBlock1, _) = unsafeChainBaseAndMicro(
          totalRefTo = keyBlock.signerData.signature,
          base = Seq(transfer3),
          micros = Nil,
          signer = secondAccount,
          version = 3,
          timestamp = blockTime
        )

        (genesisBlock, keyBlock, microBlocks, keyBlock1)
      }
    }

    def conflictingTransfersInMicro(): Gen[(Block, Block, Seq[MicroBlock], Block)] = {
      for {
        richAccount   <- accountGen
        secondAccount <- accountGen

        tsAmount = FeeAmount * 10

        blockTime = ntpNow
        transfer1 <- transfer(richAccount, secondAccount, tsAmount, validTimestampGen(blockTime))
        transfer2 <- transfer(secondAccount, richAccount, tsAmount - FeeAmount, validTimestampGen(blockTime))
        transfer3 <- transfer(secondAccount, richAccount, tsAmount - FeeAmount, validTimestampGen(blockTime))
      } yield {
        val genesisBlock = unsafeBlock(
          reference = randomSig,
          txs = Seq(GenesisTransaction.create(richAccount, tsAmount + FeeAmount, 0).explicitGet()),
          signer = TestBlock.defaultSigner,
          version = 3,
          timestamp = 0
        )

        val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
          totalRefTo = genesisBlock.signerData.signature,
          base = Seq(transfer1),
          micros = Seq(Seq(), Seq(transfer2)),
          signer = richAccount,
          version = 3,
          timestamp = blockTime
        )

        val (keyBlock1, _) = unsafeChainBaseAndMicro(
          totalRefTo = microBlocks.head.totalResBlockSig,
          base = Seq(transfer3),
          micros = Nil,
          signer = secondAccount,
          version = 3,
          timestamp = blockTime
        )

        (genesisBlock, keyBlock, microBlocks, keyBlock1)
      }
    }

    def leaseAndLeaseCancel(): Gen[(Block, Block, Block, Seq[MicroBlock], Block, KeyPair)] = {
      for {
        richAccount   <- accountGen
        secondAccount <- accountGen
        randomAccount <- accountGen

        tsAmount = FeeAmount * 10
        blockTime = ntpNow
        lease       <- lease(richAccount, secondAccount, tsAmount, validTimestampGen(blockTime))
        leaseCancel <- leaseCancel(richAccount, lease.id(), validTimestampGen(blockTime))
        transfer    <- transfer(richAccount, randomAccount, tsAmount, validTimestampGen(blockTime))
      } yield {
        val genesisBlock = unsafeBlock(
          reference = randomSig,
          txs = Seq(GenesisTransaction.create(richAccount, tsAmount + FeeAmount * 3, 0).explicitGet()),
          signer = TestBlock.defaultSigner,
          version = 3,
          timestamp = 0
        )

        val leaseBlock = unsafeBlock(
          genesisBlock.signerData.signature,
          Seq(lease),
          richAccount,
          3,
          blockTime
        )

        val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
          totalRefTo = leaseBlock.signerData.signature,
          base = Nil,
          micros = Seq(Seq(leaseCancel)),
          signer = richAccount,
          version = 3,
          timestamp = blockTime
        )

        val transferBlock = unsafeBlock(
          keyBlock.signerData.signature,
          Seq(transfer),
          secondAccount,
          3,
          blockTime
        )

        (genesisBlock, leaseBlock, keyBlock, microBlocks, transferBlock, secondAccount)
      }
    }

    def duplicateDataKeys(): Gen[(Block, Seq[Block], Seq[MicroBlock], Address)] = {
      for {
        richAccount   <- accountGen
        tsAmount = FeeAmount * 10
        blockTime = ntpNow
        data1 <- QuickTX.data(richAccount, "test", Gen.const(ntpNow))
        data2 <- QuickTX.data(richAccount, "test", Gen.const(ntpNow))
      } yield {
        val genesisBlock = unsafeBlock(
          reference = randomSig,
          txs = Seq(GenesisTransaction.create(richAccount, FeeAmount * 100, 0).explicitGet()),
          signer = TestBlock.defaultSigner,
          version = 3,
          timestamp = 0
        )

        val preBlock = unsafeBlock(
          genesisBlock.signerData.signature,
          Seq(data1),
          richAccount,
          3,
          blockTime
        )

        val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
          totalRefTo = preBlock.signerData.signature,
          base = Seq(),
          micros = Seq(Seq(data2)),
          signer = richAccount,
          version = 3,
          blockTime
        )
        (genesisBlock, Seq(preBlock, keyBlock), microBlocks, richAccount.toAddress)
      }
    }
  }
}
