package com.wavesplatform.state.diffs

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings as TFS}
import com.wavesplatform.state.diffs.BlockDiffer.{CurrentBlockFeePart, DetailedSnapshot}
import com.wavesplatform.state.{Blockchain, StateSnapshot}
import com.wavesplatform.test.*
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.{TxHelpers, TxVersion}

class BlockDifferDetailedSnapshotTest extends FreeSpec with WithState {

  private def assertDetailedSnapshot(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TFS.Enabled)(
      assertion: (StateSnapshot, DetailedSnapshot) => Unit
  ): Unit =
    withRocksDBWriter(fs) { state =>
      def differ(blockchain: Blockchain, prevBlock: Option[Block], b: Block) =
        BlockDiffer.fromBlock(blockchain, prevBlock, b, MiningConstraint.Unlimited, b.header.generationSignature)

      preconditions.foldLeft[Option[Block]](None) { (prevBlock, curBlock) =>
        val BlockDiffer.Result(snapshot, fees, totalFee, _, _, _) = differ(state, prevBlock, curBlock).explicitGet()
        state.append(snapshot, fees, totalFee, None, curBlock.header.generationSignature, curBlock)
        Some(curBlock)
      }

      val BlockDiffer.Result(snapshot, _, _, _, detailedSnapshot, _) = differ(state, preconditions.lastOption, block).explicitGet()
      assertion(snapshot, detailedSnapshot)
    }

  "BlockDiffer DetailedSnapshot" - {
    "works in case of one genesis transaction" in {
      val genesisBlock: (Address, Block) = {
        val master       = TxHelpers.signer(1)
        val genesisBlock = TestBlock.create(System.currentTimeMillis(), Seq(TxHelpers.genesis(master.toAddress)))
        (master.toAddress, genesisBlock)
      }

      val (master, b) = genesisBlock
      assertDetailedSnapshot(Seq.empty, b) { case (snapshot, DetailedSnapshot(parentSnapshot, _)) =>
        snapshot.balances((master, Waves)) shouldBe ENOUGH_AMT
        parentSnapshot.balances.get((master, Waves)) shouldBe None
        parentSnapshot.transactions.size shouldBe 1
        parentSnapshot.transactions.head._2.snapshot.balances((master, Waves)) shouldBe ENOUGH_AMT
      }
    }

    "genesis and transfers" - {
      val fee1 = 123
      val fee2 = 7777

      val amount1 = 2.waves
      val amount2 = 1.waves

      val a1 = TxHelpers.signer(1)
      val a2 = TxHelpers.signer(2)

      val genesis   = TxHelpers.genesis(a1.toAddress)
      val transfer1 = TxHelpers.transfer(a1, a2.toAddress, amount1, fee = fee1, version = TxVersion.V1)
      val transfer2 = TxHelpers.transfer(a2, a1.toAddress, amount2, fee = fee2, version = TxVersion.V1)
      val block     = TestBlock.create(a1, Seq(genesis, transfer1, transfer2))
      val address1  = a1.toAddress
      val address2  = a2.toAddress

      "transaction snapshots are correct" in {
        assertDetailedSnapshot(Seq.empty, block) { case (_, DetailedSnapshot(parentSnapshot, _)) =>
          val transactionSnapshots = parentSnapshot.transactions.map(_._2.snapshot).toSeq
          transactionSnapshots(0).balances((address1, Waves)) shouldBe ENOUGH_AMT + fee1 + fee2
          transactionSnapshots(1).balances((address1, Waves)) shouldBe ENOUGH_AMT + fee2 - amount1
          transactionSnapshots(1).balances((address2, Waves)) shouldBe amount1
          transactionSnapshots(2).balances((address1, Waves)) shouldBe ENOUGH_AMT + fee2 - amount2
          transactionSnapshots(2).balances((address2, Waves)) shouldBe amount2 - fee2
        }
      }

      "miner reward is correct" - {
        "without NG" in {
          assertDetailedSnapshot(Seq.empty, block) { case (_, DetailedSnapshot(parentSnapshot, _)) =>
            parentSnapshot.balances((address1, Waves)) shouldBe fee1 + fee2
          }
        }

        "with NG" - {
          val ngFs = TFS.Enabled.copy(preActivatedFeatures = TFS.Enabled.preActivatedFeatures + (BlockchainFeatures.NG.id -> 0))

          "no history — only 40% from current block" in {
            assertDetailedSnapshot(Seq.empty, block, ngFs) { case (_, DetailedSnapshot(parentSnapshot, _)) =>
              parentSnapshot.balances((address1, Waves)) shouldBe ENOUGH_AMT + fee1 / 5 * 2 + fee2 / 5 * 2
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
              val transfer1 = TxHelpers.transfer(a1, a2.toAddress, amount1, fee = fee1, version = TxVersion.V1)
              val transfer2 = TxHelpers.transfer(a2, a1.toAddress, amount2, fee = fee2, version = TxVersion.V1)

              val history = Seq(
                TestBlock.create(a1, Seq(genesis)),
                TestBlock.create(a1, Seq(transfer1))
              )
              val block = TestBlock.create(miner, Seq(transfer2))

              (history, block, miner.toAddress)
            }

            val (history, block, ngMiner) = blocksNgMiner
            assertDetailedSnapshot(history, block, ngFs) { case (_, DetailedSnapshot(parentSnapshot, _)) =>
              parentSnapshot.balances((ngMiner, Waves)) shouldBe (fee1 - fee1 / 5 * 2) + fee2 / 5 * 2
            }
          }
        }
      }
    }
  }
}
