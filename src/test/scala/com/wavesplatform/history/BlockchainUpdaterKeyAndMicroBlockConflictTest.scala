package com.wavesplatform.history

import com.wavesplatform._
import com.wavesplatform.account.{AddressOrAlias, PrivateKeyAccount}
import com.wavesplatform.block.{Block, MicroBlock, SignerData}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.lease.{LeaseCancelTransactionV1, LeaseTransactionV1}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class BlockchainUpdaterKeyAndMicroBlockConflictTest extends PropSpec with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {

  private def preconditionsAndPayments(): Gen[(Block, Block, Seq[MicroBlock], Block)] =
    for {
      richAccount        <- accountGen
      minerAccount       <- accountGen
      lease <- validLeaseGen(richAccount, minerAccount.toAddress)
      leaseCancel <- validLeaseCancelGen(richAccount, lease.id())
      transfer <- validTransferGen(richAccount, ENOUGH_AMT - (lease.amount - lease.fee) * 2)
    } yield {
      val genesisBlock = unsafeBlock(
        reference = randomSig,
        txs = Seq(GenesisTransaction.create(richAccount, ENOUGH_AMT, 0).explicitGet()),
        signer = TestBlock.defaultSigner,
        version = 3,
        timestamp = 0
      )

      val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
        totalRefTo = genesisBlock.signerData.signature,
        base = Seq(lease),
        micros = Seq(Seq(leaseCancel)),
        signer = TestBlock.defaultSigner,
        version = 3,
        timestamp = System.currentTimeMillis()
      )

      val (keyBlock1, _) = unsafeChainBaseAndMicro(
        totalRefTo = genesisBlock.signerData.signature,
        base = Seq(transfer),
        micros = Nil,
        signer = minerAccount,
        version = 3,
        timestamp = System.currentTimeMillis()
      )

      (genesisBlock, keyBlock, microBlocks, keyBlock1)
    }

  property("new key block should be validated to previous") {
    forAll(preconditionsAndPayments()) {
      case (prevBlock, keyBlock, microBlocks, keyBlock1) =>
        withDomain(MicroblocksActivatedAt0WavesSettings) { d =>
          val blocksApplied = for {
            _ <- d.blockchainUpdater.processBlock(prevBlock)
            _ <- d.blockchainUpdater.processBlock(keyBlock)
          } yield ()

          val r = microBlocks.foldLeft(blocksApplied) {
            case (Right(_), curr) => d.blockchainUpdater.processMicroBlock(curr)
            case (x, _)           => x
          }

          d.blockchainUpdater.processBlock(keyBlock1) shouldBe 'right
        }
    }
  }

  private def validTransferGen(from: PrivateKeyAccount, amount: Long): Gen[Transaction] =
    for {
      feeAmount <- smallFeeGen
      timestamp <- timestampGen
      recipient <- accountGen
    } yield TransferTransactionV1.selfSigned(None, from, recipient, amount, timestamp, None, feeAmount, Array.empty).explicitGet()

  private def validLeaseGen(from: PrivateKeyAccount, to: AddressOrAlias): Gen[LeaseTransactionV1] =
    for {
      amount    <- smallFeeGen
      feeAmount <- smallFeeGen
      timestamp <- timestampGen
    } yield LeaseTransactionV1.selfSigned(from, amount, feeAmount, timestamp, to).explicitGet()

  private def validLeaseCancelGen(from: PrivateKeyAccount, leaseId: ByteStr): Gen[LeaseCancelTransactionV1] =
    for {
      amount    <- smallFeeGen
      feeAmount <- smallFeeGen
      timestamp <- timestampGen
    } yield LeaseCancelTransactionV1.selfSigned(from, leaseId, feeAmount, timestamp).explicitGet()

  private def unsafeChainBaseAndMicro(totalRefTo: ByteStr,
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

  private def unsafeMicro(totalRefTo: ByteStr,
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

  private def unsafeBlock(reference: ByteStr,
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
