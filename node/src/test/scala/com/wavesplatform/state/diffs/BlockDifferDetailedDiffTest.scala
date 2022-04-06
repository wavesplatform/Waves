package com.wavesplatform.state.diffs

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings as TFS}
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.{TxHelpers, TxVersion}
import com.wavesplatform.test.*

class BlockDifferDetailedDiffTest extends FreeSpec with WithState {

  private def assertDetailedDiff(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TFS.Enabled)(
      assertion: (Diff, DetailedDiff) => Unit
  ): Unit =
    withLevelDBWriter(fs) { state =>
      def differ(blockchain: Blockchain, prevBlock: Option[Block], b: Block) =
        BlockDiffer.fromBlock(blockchain, prevBlock, b, MiningConstraint.Unlimited, b.header.generationSignature)

      preconditions.foldLeft[Option[Block]](None) { (prevBlock, curBlock) =>
        val BlockDiffer.Result(diff, fees, totalFee, _, _) = differ(state, prevBlock, curBlock).explicitGet()
        state.append(diff, fees, totalFee, None, curBlock.header.generationSignature, curBlock)
        Some(curBlock)
      }

      val BlockDiffer.Result(diff, _, _, _, detailedDiff) = differ(state, preconditions.lastOption, block).explicitGet()
      assertion(diff, detailedDiff)
    }

  "BlockDiffer DetailedDiff" - {
    "works in case of one genesis transaction" in {
      val genesisBlock: (Address, Block) = {
        val master       = TxHelpers.signer(1)
        val genesisBlock = TestBlock.create(System.currentTimeMillis(), Seq(TxHelpers.genesis(master.toAddress)))
        (master.toAddress, genesisBlock)
      }

      val (master, b) = genesisBlock
      assertDetailedDiff(Seq.empty, b) {
        case (diff, DetailedDiff(parentDiff, transactionDiffs)) =>
          diff.portfolios(master).balance shouldBe ENOUGH_AMT
          parentDiff.portfolios.get(master) shouldBe None
          transactionDiffs.length shouldBe 1
          transactionDiffs.head.portfolios(master).balance shouldBe ENOUGH_AMT
      }
    }

    "genesis and transfers" - {
      val transactionFee = 10L

      val genesisTransfersBlock: (Address, Address, Long, Long, Block) = {
        val a1 = TxHelpers.signer(1)
        val a2 = TxHelpers.signer(2)

        val amount1 = 2.waves
        val amount2 = 1.waves

        val genesis   = TxHelpers.genesis(a1.toAddress)
        val transfer1 = TxHelpers.transfer(a1, a2.toAddress, amount1, fee = transactionFee, version = TxVersion.V1)
        val transfer2 = TxHelpers.transfer(a2, a1.toAddress, amount2, fee = transactionFee, version = TxVersion.V1)
        val block     = TestBlock.create(a1, Seq(genesis, transfer1, transfer2))

        (a1.toAddress, a2.toAddress, amount1, amount2, block)
      }

      "transaction diffs are correct" in {
        val (addr1, addr2, amt1, amt2, b) = genesisTransfersBlock
        assertDetailedDiff(Seq.empty, b) {
          case (_, DetailedDiff(_, td)) =>
            val transactionDiffs = td.reverse
            transactionDiffs.head.portfolios(addr1).balance shouldBe ENOUGH_AMT
            transactionDiffs(1).portfolios(addr1).balance shouldBe -(amt1 + transactionFee)
            transactionDiffs(1).portfolios(addr2).balance shouldBe amt1
            transactionDiffs(2).portfolios(addr2).balance shouldBe -(amt2 + transactionFee)
            transactionDiffs(2).portfolios(addr1).balance shouldBe amt2
        }
      }

      "miner reward is correct" - {
        "without NG" in {
          val (addr1, _, _, _, b) = genesisTransfersBlock
          assertDetailedDiff(Seq.empty, b) {
            case (_, DetailedDiff(parentDiff, _)) =>
              parentDiff.portfolios(addr1).balance shouldBe 20
          }
        }

        "with NG" - {
          val ngFs = TFS.Enabled.copy(preActivatedFeatures = TFS.Enabled.preActivatedFeatures + (BlockchainFeatures.NG.id -> 0))

          "no history — only 40% from current block" in {
            val (addr1, _, _, _, b) = genesisTransfersBlock
            assertDetailedDiff(Seq.empty, b, ngFs) {
              case (_, DetailedDiff(parentDiff, _)) =>
                parentDiff.portfolios(addr1).balance shouldBe (transactionFee * 2 * 0.4) // 40%
            }
          }

          "with history — 60% from last + 40% from current block" in {
            val blocksNgMiner: (Seq[Block], Block, Address) = {
              val a1    = TxHelpers.signer(1)
              val a2    = TxHelpers.signer(2)
              val miner = TxHelpers.signer(3)

              val amount1 = 2.waves
              val amount2 = 1.waves

              val genesis   = TxHelpers.genesis(a1.toAddress)
              val transfer1 = TxHelpers.transfer(a1, a2.toAddress, amount1, fee = transactionFee, version = TxVersion.V1)
              val transfer2 = TxHelpers.transfer(a2, a1.toAddress, amount2, fee = transactionFee, version = TxVersion.V1)

              val history = Seq(
                TestBlock.create(a1, Seq(genesis)),
                TestBlock.create(a1, Seq(transfer1))
              )
              val block = TestBlock.create(miner, Seq(transfer2))

              (history, block, miner.toAddress)
            }

            val (history, block, ngMiner) = blocksNgMiner
            assertDetailedDiff(history, block, ngFs) {
              case (_, DetailedDiff(parentDiff, _)) =>
                parentDiff.portfolios(ngMiner).balance shouldBe transactionFee // 60% + 40%
            }
          }
        }
      }
    }
  }
}
