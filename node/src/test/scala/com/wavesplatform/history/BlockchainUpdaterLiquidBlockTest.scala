package com.wavesplatform.history

import com.wavesplatform._
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.state.diffs.ENOUGH_AMT
import com.wavesplatform.transaction.GenesisTransaction
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class BlockchainUpdaterLiquidBlockTest
    extends PropSpec
    with PropertyChecks
    with DomainScenarioDrivenPropertyCheck
    with Matchers
    with TransactionGen
    with BlocksTransactionsHelpers
    with NoShrink {
  import QuickTX._
  import UnsafeBlocks._

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
        txs = Seq(GenesisTransaction.create(richAccount, ENOUGH_AMT, 0).explicitGet()),
        signer = TestBlock.defaultSigner,
        version = 3,
        timestamp = 0
      )

      val (keyBlock, microBlocks) = unsafeChainBaseAndMicro(
        totalRefTo = prevBlock.signature,
        base = keyBlockTxs,
        micros = microTxs.grouped(math.max(1, txNumberInMicros / 5)).toSeq,
        signer = TestBlock.defaultSigner,
        version = 3,
        timestamp = ntpNow
      )

      (prevBlock, keyBlock, microBlocks)
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
          d.appendBlock(genBlock)
          d.appendBlock(keyBlock)
          microBlocks.foreach { mb =>
            d.blockchainUpdater.processMicroBlock(mb) shouldBe 'right
          }
        }
    }
  }
}
