package com.wavesplatform.history

import com.wavesplatform.*
import com.wavesplatform.account.{Address, KeyPair}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.GenesisTransaction
import org.scalacheck.Gen
import org.scalatest.*

class BlockchainUpdaterKeyAndMicroBlockConflictTest
    extends PropSpec
    with DomainScenarioDrivenPropertyCheck
    with OptionValues
    with BlocksTransactionsHelpers {

  property("new key block should be validated to previous") {
    forAll(Preconditions.conflictingTransfers()) { case (prevBlock, keyBlock, microBlocks, keyBlock1) =>
      withDomain(MicroblocksActivatedAt0WavesSettings) { d =>
        d.blockchainUpdater.processBlock(prevBlock) should beRight
        d.blockchainUpdater.processBlock(keyBlock) should beRight

        microBlocks.foreach(d.blockchainUpdater.processMicroBlock(_, None) should beRight)

        d.blockchainUpdater.processBlock(keyBlock1) should beRight
      }
    }

    forAll(Preconditions.conflictingTransfersInMicro()) { case (prevBlock, keyBlock, microBlocks, keyBlock1) =>
      withDomain(MicroblocksActivatedAt0WavesSettings) { d =>
        d.blockchainUpdater.processBlock(prevBlock) should beRight
        d.blockchainUpdater.processBlock(keyBlock) should beRight

        microBlocks.foreach(d.blockchainUpdater.processMicroBlock(_, None) should beRight)

        d.blockchainUpdater.processBlock(keyBlock1) should beRight
      }
    }

    forAll(Preconditions.leaseAndLeaseCancel()) { case (genesisBlock, leaseBlock, keyBlock, microBlocks, transferBlock, secondAccount) =>
      withDomain(MicroblocksActivatedAt0WavesSettings) { d =>
        Seq(genesisBlock, leaseBlock, keyBlock).foreach(d.blockchainUpdater.processBlock(_) should beRight)
        assert(d.blockchainUpdater.effectiveBalance(secondAccount.toAddress, 0) > 0)

        microBlocks.foreach(d.blockchainUpdater.processMicroBlock(_, None) should beRight)
        assert(d.blockchainUpdater.effectiveBalance(secondAccount.toAddress, 0, Some(leaseBlock.id())) > 0)

        assert(d.blockchainUpdater.processBlock(transferBlock).toString.contains("negative effective balance"))
      }
    }
  }

  property("data keys should not be duplicated") {
    forAll(Preconditions.duplicateDataKeys()) { case (genesisBlock, blocks, microBlocks, address) =>
      withDomain(DataAndMicroblocksActivatedAt0WavesSettings) { d =>
        Seq(genesisBlock, blocks(0), blocks(1)).foreach(d.blockchainUpdater.processBlock(_) should beRight)
        d.blockchainUpdater.accountData(address, "test") shouldBe defined
        microBlocks.foreach(d.blockchainUpdater.processMicroBlock(_, None) should beRight)
        d.blockchainUpdater.accountData(address, "test") shouldBe defined
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
        transfer1 <- transfer(richAccount, secondAccount.toAddress, tsAmount, validTimestampGen(blockTime))
        transfer2 <- transfer(secondAccount, richAccount.toAddress, tsAmount - FeeAmount, validTimestampGen(blockTime))
        transfer3 <- transfer(secondAccount, richAccount.toAddress, tsAmount - FeeAmount, validTimestampGen(blockTime))
      } yield {
        val genesisBlock = unsafeBlock(
          reference = randomSig,
          txs = Seq(GenesisTransaction.create(richAccount.toAddress, tsAmount + FeeAmount, 0).explicitGet()),
          signer = TestBlock.defaultSigner,
          version = 3,
          timestamp = 0
        )

        val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
          totalRefTo = genesisBlock.signature,
          base = Seq(transfer1),
          micros = Seq(Seq(transfer2)),
          signer = richAccount,
          version = 3,
          timestamp = blockTime
        )

        val (keyBlock1, _) = unsafeChainBaseAndMicro(
          totalRefTo = keyBlock.signature,
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
        transfer1 <- transfer(richAccount, secondAccount.toAddress, tsAmount, validTimestampGen(blockTime))
        transfer2 <- transfer(secondAccount, richAccount.toAddress, tsAmount - FeeAmount, validTimestampGen(blockTime))
        transfer3 <- transfer(secondAccount, richAccount.toAddress, tsAmount - FeeAmount, validTimestampGen(blockTime))
      } yield {
        val genesisBlock = unsafeBlock(
          reference = randomSig,
          txs = Seq(GenesisTransaction.create(richAccount.toAddress, tsAmount + FeeAmount, 0).explicitGet()),
          signer = TestBlock.defaultSigner,
          version = 3,
          timestamp = 0
        )

        val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
          totalRefTo = genesisBlock.signature,
          base = Seq(transfer1),
          micros = Seq(Seq(transfer2)),
          signer = richAccount,
          version = 3,
          timestamp = blockTime
        )

        val (keyBlock1, _) = unsafeChainBaseAndMicro(
          totalRefTo = keyBlock.id(),
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

        tsAmount  = FeeAmount * 10
        blockTime = ntpNow
        lease       <- lease(richAccount, secondAccount.toAddress, tsAmount, validTimestampGen(blockTime))
        leaseCancel <- leaseCancel(richAccount, lease.id(), validTimestampGen(blockTime))
        transfer    <- transfer(richAccount, randomAccount.toAddress, tsAmount, validTimestampGen(blockTime))
      } yield {
        val genesisBlock = unsafeBlock(
          reference = randomSig,
          txs = Seq(GenesisTransaction.create(richAccount.toAddress, tsAmount + FeeAmount * 3, 0).explicitGet()),
          signer = TestBlock.defaultSigner,
          version = 3,
          timestamp = 0
        )

        val leaseBlock = unsafeBlock(
          genesisBlock.signature,
          Seq(lease),
          richAccount,
          3,
          blockTime
        )

        val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
          totalRefTo = leaseBlock.signature,
          base = Nil,
          micros = Seq(Seq(leaseCancel)),
          signer = richAccount,
          version = 3,
          timestamp = blockTime
        )

        val transferBlock = unsafeBlock(
          keyBlock.signature,
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
        richAccount <- accountGen
        blockTime = ntpNow
        data1 <- QuickTX.data(richAccount, "test", Gen.const(ntpNow))
        data2 <- QuickTX.data(richAccount, "test", Gen.const(ntpNow))
      } yield {
        val genesisBlock = unsafeBlock(
          reference = randomSig,
          txs = Seq(GenesisTransaction.create(richAccount.toAddress, FeeAmount * 100, 0).explicitGet()),
          signer = TestBlock.defaultSigner,
          version = 3,
          timestamp = 0
        )

        val preBlock = unsafeBlock(
          genesisBlock.signature,
          Seq(data1),
          richAccount,
          3,
          blockTime
        )

        val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
          totalRefTo = preBlock.signature,
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
