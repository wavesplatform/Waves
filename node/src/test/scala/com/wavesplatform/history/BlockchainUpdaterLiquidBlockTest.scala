package com.wavesplatform.history

import com.wavesplatform.*
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.history.Domain.BlockchainUpdaterExt
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.test.PropSpec
import com.wavesplatform.transaction.GenesisTransaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import org.scalacheck.Gen

class BlockchainUpdaterLiquidBlockTest extends PropSpec with DomainScenarioDrivenPropertyCheck with BlocksTransactionsHelpers {
  import QuickTX.*
  import UnsafeBlocks.*

  private def preconditionsAndPayments(minTx: Int, maxTx: Int): Gen[(Block, Block, Seq[MicroBlock])] =
    for {
      richAccount        <- accountGen
      totalTxNumber      <- Gen.chooseNum(minTx, maxTx)
      txNumberInKeyBlock <- Gen.chooseNum(0, Block.MaxTransactionsPerBlockVer3)
      allTxs             <- Gen.listOfN(totalTxNumber, transfer(richAccount, timestamp = Gen.delay(Gen.const(ntpTime.getTimestamp()))))
    } yield {
      val (keyBlockTxs, microTxs) = allTxs.splitAt(txNumberInKeyBlock)
      val txNumberInMicros        = totalTxNumber - txNumberInKeyBlock

      val prevBlock = unsafeBlock(
        reference = randomSig,
        txs = Seq(GenesisTransaction.create(richAccount.toAddress, ENOUGH_AMT, 0).explicitGet()),
        signer = TestBlock.defaultSigner,
        version = 3,
        timestamp = 0
      )

      val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
        totalRefTo = prevBlock.signature,
        base = keyBlockTxs,
        micros = microTxs.grouped((txNumberInMicros / 5) min 500 max 1).toSeq,
        signer = TestBlock.defaultSigner,
        version = 3,
        timestamp = ntpNow
      )

      (prevBlock, keyBlock, microBlocks)
    }

  property("liquid block can't be overfilled") {
    import Block.{MaxTransactionsPerBlockVer3 => Max}
    forAll(preconditionsAndPayments(Max + 1, Max + 100)) { case (prevBlock, keyBlock, microBlocks) =>
      withDomain(MicroblocksActivatedAt0WavesSettings) { d =>
        val blocksApplied = for {
          _ <- d.blockchainUpdater.processBlock(prevBlock)
          _ <- d.blockchainUpdater.processBlock(keyBlock)
        } yield ()

        val r = microBlocks.foldLeft(blocksApplied) {
          case (Right(_), curr) => d.blockchainUpdater.processMicroBlock(curr, None).map(_ => ())
          case (x, _)           => x
        }

        withClue("All microblocks should not be processed") {
          r match {
            case Left(e: GenericError) => e.err should include("Limit of txs was reached")
            case x =>
              val txNumberByMicroBlock = microBlocks.map(_.transactionData.size)
              fail(
                s"Unexpected result: $x. keyblock txs: ${keyBlock.transactionData.length}, " +
                  s"microblock txs: ${txNumberByMicroBlock.mkString(", ")} (total: ${txNumberByMicroBlock.sum}), " +
                  s"total txs: ${keyBlock.transactionData.length + txNumberByMicroBlock.sum}"
              )
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
    forAll(preconditionsAndPayments(10, Block.MaxTransactionsPerBlockVer3)) { case (genBlock, keyBlock, microBlocks) =>
      withDomain(oneTxPerMicroSettings) { d =>
        d.blockchainUpdater.processBlock(genBlock)
        d.blockchainUpdater.processBlock(keyBlock)
        microBlocks.foreach { mb =>
          d.blockchainUpdater.processMicroBlock(mb, None) should beRight
        }
      }
    }
  }
}
