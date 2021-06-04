package com.wavesplatform.state.diffs

import com.wavesplatform.BlockGen
import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings => TFS}
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.GenesisTransaction
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class BlockDifferDetailedDiffTest extends FreeSpec with Matchers with PropertyChecks with BlockGen with WithState {

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
      val genesisBlockGen: Gen[(Address, Block)] = for {
        master <- accountGen
        ts     <- positiveIntGen
        genesisBlock = TestBlock
          .create(ts, Seq(GenesisTransaction.create(master.toAddress, ENOUGH_AMT, ts).explicitGet()))
      } yield (master.toAddress, genesisBlock)

      forAll(genesisBlockGen) {
        case (master, b) =>
          assertDetailedDiff(Seq.empty, b) {
            case (diff, DetailedDiff(parentDiff, transactionDiffs)) =>
              diff.portfolios(master).balance shouldBe ENOUGH_AMT
              parentDiff.portfolios.get(master) shouldBe None
              transactionDiffs.length shouldBe 1
              transactionDiffs.head.portfolios(master).balance shouldBe ENOUGH_AMT
          }
      }
    }

    "genesis and transfers" - {
      val transactionFee = 10L

      val genesisTransfersBlockGen: Gen[(Address, Address, Long, Long, Block)] = for {
        a1 <- accountGen
        a2 <- accountGen suchThat (_ != a1)
        ts <- positiveIntGen

        amount1 <- Gen.choose[Long](2, ENOUGH_AMT - 1)
        amount2 <- Gen.choose[Long](1, amount1 - 1)

        genesis   = GenesisTransaction.create(a1.toAddress, ENOUGH_AMT, ts).explicitGet()
        transfer1 = createWavesTransfer(a1, a2.toAddress, amount1, transactionFee, ts + 10).explicitGet()
        transfer2 = createWavesTransfer(a2, a1.toAddress, amount2, transactionFee, ts + 20).explicitGet()
        block = TestBlock
          .create(
            a1,
            Seq(
              genesis,
              transfer1,
              transfer2
            )
          )
      } yield (a1.toAddress, a2.toAddress, amount1, amount2, block)

      "transaction diffs are correct" in
        forAll(genesisTransfersBlockGen) {
          case (addr1, addr2, amt1, amt2, b) =>
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
        "without NG" in
          forAll(genesisTransfersBlockGen) {
            case (addr1, _, _, _, b) =>
              assertDetailedDiff(Seq.empty, b) {
                case (_, DetailedDiff(parentDiff, __)) =>
                  parentDiff.portfolios(addr1).balance shouldBe 20
              }
          }

        "with NG" - {
          val ngFs = TFS.Enabled.copy(preActivatedFeatures = TFS.Enabled.preActivatedFeatures + (BlockchainFeatures.NG.id -> 0))

          "no history — only 40% from current block" in
            forAll(genesisTransfersBlockGen) {
              case (addr1, _, _, _, b) =>
                assertDetailedDiff(Seq.empty, b, ngFs) {
                  case (_, DetailedDiff(parentDiff, _)) =>
                    parentDiff.portfolios(addr1).balance shouldBe (transactionFee * 2 * 0.4) // 40%
                }
            }

          "with history — 60% from last + 40% from current block" in {
            val blocksNgMinerGen: Gen[(Seq[Block], Block, Address)] = for {
              a1    <- accountGen
              a2    <- accountGen suchThat (_ != a1)
              miner <- accountGen suchThat (addr => addr != a1 && addr != a2)

              ts <- positiveIntGen

              amount1 <- Gen.choose[Long](2, ENOUGH_AMT - 1)
              amount2 <- Gen.choose[Long](1, amount1 - 1)

              genesis   = GenesisTransaction.create(a1.toAddress, ENOUGH_AMT, ts).explicitGet()
              transfer1 = createWavesTransfer(a1, a2.toAddress, amount1, transactionFee, ts + 10).explicitGet()
              transfer2 = createWavesTransfer(a2, a1.toAddress, amount2, transactionFee, ts + 20).explicitGet()
              history = Seq(
                TestBlock.create(a1, Seq(genesis)),
                TestBlock.create(a1, Seq(transfer1))
              )
              block = TestBlock.create(miner, Seq(transfer2))
            } yield (history, block, miner.toAddress)

            forAll(blocksNgMinerGen) {
              case (history, block, ngMiner) =>
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
}
