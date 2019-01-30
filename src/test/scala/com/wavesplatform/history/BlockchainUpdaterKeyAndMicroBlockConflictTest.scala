package com.wavesplatform.history

import com.wavesplatform._
import com.wavesplatform.account.{AddressOrAlias, PrivateKeyAccount}
import com.wavesplatform.block.{Block, MicroBlock, SignerData}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.transaction.lease.{LeaseCancelTransactionV1, LeaseTransactionV1}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class BlockchainUpdaterKeyAndMicroBlockConflictTest extends PropSpec with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {
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

    forAll(Preconditions.leaseAndLeaseCancel()) {
      case (genesisBlock, leaseBlock, keyBlock, microBlocks, transferBlock, secondAccount) =>
        withDomain(MicroblocksActivatedAt0WavesSettings) { d =>
          Seq(genesisBlock, leaseBlock, keyBlock).foreach(d.blockchainUpdater.processBlock(_) shouldBe 'right)
          assert(d.blockchainUpdater.effectiveBalance(secondAccount, d.blockchainUpdater.height, 0) > 0)

          microBlocks.foreach(d.blockchainUpdater.processMicroBlock(_) shouldBe 'right)
          assert(d.blockchainUpdater.effectiveBalance(secondAccount, d.blockchainUpdater.height, 0) > 0)

          assert(d.blockchainUpdater.processBlock(transferBlock).toString.contains("negative effective balance"))
        }
    }
  }

  private[this] object TX {
    val FeeAmount = 400000

    def validTransferGen(from: PrivateKeyAccount, to: AddressOrAlias, amount: Long): Gen[Transaction] =
      for {
        timestamp <- timestampGen
      } yield TransferTransactionV1.selfSigned(None, from, to, amount, timestamp, None, FeeAmount, Array.empty).explicitGet()

    def validLeaseGen(from: PrivateKeyAccount, to: AddressOrAlias, amount: Long): Gen[LeaseTransactionV1] =
      for {
        timestamp <- timestampGen
      } yield LeaseTransactionV1.selfSigned(from, amount, FeeAmount, timestamp, to).explicitGet()

    def validLeaseCancelGen(from: PrivateKeyAccount, leaseId: ByteStr): Gen[LeaseCancelTransactionV1] =
      for {
        timestamp <- timestampGen
      } yield LeaseCancelTransactionV1.selfSigned(from, leaseId, FeeAmount, timestamp).explicitGet()
  }

  private[this] object Preconditions {
    import TX._

    def conflictingTransfers(): Gen[(Block, Block, Seq[MicroBlock], Block)] = {
      for {
        richAccount   <- accountGen
        secondAccount <- accountGen

        tsAmount  = FeeAmount * 10
        transfer1 <- validTransferGen(richAccount, secondAccount, tsAmount)
        transfer2 <- validTransferGen(secondAccount, richAccount, tsAmount - FeeAmount)
        transfer3 <- validTransferGen(secondAccount, richAccount, tsAmount - FeeAmount)
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
          timestamp = System.currentTimeMillis()
        )

        val (keyBlock1, _) = unsafeChainBaseAndMicro(
          totalRefTo = keyBlock.signerData.signature,
          base = Seq(transfer3),
          micros = Nil,
          signer = secondAccount,
          version = 3,
          timestamp = System.currentTimeMillis()
        )

        (genesisBlock, keyBlock, microBlocks, keyBlock1)
      }
    }

    def leaseAndLeaseCancel(): Gen[(Block, Block, Block, Seq[MicroBlock], Block, PrivateKeyAccount)] = {
      for {
        richAccount   <- accountGen
        secondAccount <- accountGen
        randomAccount <- accountGen

        tsAmount  = FeeAmount * 10
        lease <- validLeaseGen(richAccount, secondAccount, tsAmount)
        leaseCancel <- validLeaseCancelGen(richAccount, lease.id())
        transfer <- validTransferGen(richAccount, randomAccount, tsAmount)
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
          System.currentTimeMillis()
        )

        val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
          totalRefTo = leaseBlock.signerData.signature,
          base = Nil,
          micros = Seq(Seq(leaseCancel)),
          signer = richAccount,
          version = 3,
          timestamp = System.currentTimeMillis()
        )

        val transferBlock = unsafeBlock(
          keyBlock.signerData.signature,
          Seq(transfer),
          secondAccount,
          3,
          System.currentTimeMillis()
        )

        (genesisBlock, leaseBlock, keyBlock, microBlocks, transferBlock, secondAccount)
      }
    }

    private[this] def unsafeChainBaseAndMicro(totalRefTo: ByteStr,
                                              base: Seq[Transaction],
                                              micros: Seq[Seq[Transaction]],
                                              signer: PrivateKeyAccount,
                                              version: Byte,
                                              timestamp: Long): (Block, Seq[MicroBlock]) = {
      val block = unsafeBlock(totalRefTo, base, signer, version, timestamp)
      val microBlocks = micros
        .foldLeft((block, Seq.empty[MicroBlock])) {
          case ((lastTotal, allMicros), txs) =>
            val (newTotal, micro) = unsafeMicro(totalRefTo, lastTotal, txs, signer, version, timestamp)
            (newTotal, allMicros :+ micro)
        }
        ._2
      (block, microBlocks)
    }

    private[this] def unsafeMicro(totalRefTo: ByteStr,
                                  prevTotal: Block,
                                  txs: Seq[Transaction],
                                  signer: PrivateKeyAccount,
                                  version: Byte,
                                  ts: Long): (Block, MicroBlock) = {
      val newTotalBlock = unsafeBlock(totalRefTo, prevTotal.transactionData ++ txs, signer, version, ts)
      val unsigned      = new MicroBlock(version, signer, txs, prevTotal.uniqueId, newTotalBlock.uniqueId, ByteStr.empty)
      val signature     = crypto.sign(signer, unsigned.bytes())
      val signed        = unsigned.copy(signature = ByteStr(signature))
      (newTotalBlock, signed)
    }

    private[this] def unsafeBlock(reference: ByteStr,
                                  txs: Seq[Transaction],
                                  signer: PrivateKeyAccount,
                                  version: Byte,
                                  timestamp: Long,
                                  bTarget: Long = DefaultBaseTarget): Block = {
      val unsigned = Block(
        version = version,
        timestamp = timestamp,
        reference = reference,
        consensusData = NxtLikeConsensusBlockData(
          baseTarget = bTarget,
          generationSignature = generationSignature
        ),
        transactionData = txs,
        signerData = SignerData(
          generator = signer,
          signature = ByteStr.empty
        ),
        featureVotes = Set.empty
      )

      unsigned.copy(signerData = SignerData(signer, ByteStr(crypto.sign(signer, unsigned.bytes()))))
    }
  }
}
