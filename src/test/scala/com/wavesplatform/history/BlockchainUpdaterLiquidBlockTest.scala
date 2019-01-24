package com.wavesplatform.history

import com.wavesplatform._
import com.wavesplatform.account.PrivateKeyAccount
import com.wavesplatform.block.{Block, MicroBlock, SignerData}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.consensus.nxt.NxtLikeConsensusBlockData
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.ValidationError.GenericError
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.transaction.{GenesisTransaction, Transaction}
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class BlockchainUpdaterLiquidBlockTest extends PropSpec with PropertyChecks with DomainScenarioDrivenPropertyCheck with Matchers with TransactionGen {

  private def preconditionsAndPayments(minTx: Int, maxTx: Int): Gen[(Block, Block, Seq[MicroBlock])] =
    for {
      richAccount        <- accountGen
      totalTxNumber      <- Gen.chooseNum(minTx, maxTx)
      txNumberInKeyBlock <- Gen.chooseNum(0, Block.MaxTransactionsPerBlockVer3)
      allTxs             <- Gen.listOfN(totalTxNumber, validTransferGen(richAccount))
    } yield {
      val (keyBlockTxs, microTxs) = allTxs.splitAt(txNumberInKeyBlock)
      val txNumberInMicros        = totalTxNumber - txNumberInKeyBlock

      val prevBlock = unsafeBlock(
        reference = randomSig,
        txs = Seq(GenesisTransaction.create(richAccount, ENOUGH_AMT, 0).explicitGet()),
        signer = TestBlock.defaultSigner,
        version = 3,
        timestamp = 0
      )

      val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
        totalRefTo = prevBlock.signerData.signature,
        base = keyBlockTxs,
        micros = microTxs.grouped(math.max(1, txNumberInMicros / 5)).toSeq,
        signer = TestBlock.defaultSigner,
        version = 3,
        timestamp = System.currentTimeMillis()
      )

      (prevBlock, keyBlock, microBlocks)
    }

  property("liquid block can't be overfilled") {
    import Block.{MaxTransactionsPerBlockVer3 => Max}
    forAll(preconditionsAndPayments(Max + 1, Max + 100)) {
      case (prevBlock, keyBlock, microBlocks) =>
        withDomain(MicroblocksActivatedAt0WavesSettings) { d =>
          val blocksApplied = for {
            _ <- d.blockchainUpdater.processBlock(prevBlock)
            _ <- d.blockchainUpdater.processBlock(keyBlock)
          } yield ()

          val r = microBlocks.foldLeft(blocksApplied) {
            case (Right(_), curr) => d.blockchainUpdater.processMicroBlock(curr)
            case (x, _)           => x
          }

          withClue("All microblocks should not be processed") {
            r match {
              case Left(e: GenericError) => e.err should include("Limit of txs was reached")
              case x =>
                val txNumberByMicroBlock = microBlocks.map(_.transactionData.size)
                fail(
                  s"Unexpected result: $x. keyblock txs: ${keyBlock.transactionCount}, " +
                    s"microblock txs: ${txNumberByMicroBlock.mkString(", ")} (total: ${txNumberByMicroBlock.sum}), " +
                    s"total txs: ${keyBlock.transactionCount + txNumberByMicroBlock.sum}")
            }
          }
        }
    }
  }

  property("miner settings don't interfere with micro block processing") {
    val oneTxPerMicroSettings = MicroblocksActivatedAt0WavesSettings
      .copy(
        minerSettings = MicroblocksActivatedAt0WavesSettings.minerSettings.copy(
          maxTransactionsInMicroBlock = 1
        )
      )
    forAll(preconditionsAndPayments(10, Block.MaxTransactionsPerBlockVer3)) {
      case (genBlock, keyBlock, microBlocks) =>
        withDomain(oneTxPerMicroSettings) { d =>
          d.blockchainUpdater.processBlock(genBlock)
          d.blockchainUpdater.processBlock(keyBlock)
          microBlocks.foreach { mb =>
            d.blockchainUpdater.processMicroBlock(mb) shouldBe 'right
          }
        }
    }
  }

  private def validTransferGen(from: PrivateKeyAccount): Gen[Transaction] =
    for {
      amount    <- smallFeeGen
      feeAmount <- smallFeeGen
      timestamp <- timestampGen
      recipient <- accountGen
    } yield TransferTransactionV1.selfSigned(None, from, recipient, amount, timestamp, None, feeAmount, Array.empty).explicitGet()

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
