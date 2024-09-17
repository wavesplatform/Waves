package com.wavesplatform.state.diffs

import com.wavesplatform.account.Address
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.{WithDomain, WithState}
import com.wavesplatform.history.defaultSigner
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.WavesSettings
import com.wavesplatform.state.StateSnapshot
import com.wavesplatform.test.*
import com.wavesplatform.test.DomainPresets.{NG, RideV6, SettingsFromDefaultConfig}
import com.wavesplatform.transaction.Asset.Waves
import com.wavesplatform.transaction.TxHelpers.defaultAddress
import com.wavesplatform.transaction.{TxHelpers, TxVersion}

class BlockDifferDetailedSnapshotTest extends FreeSpec with WithState with WithDomain {
  private def assertDetailedSnapshot(block: Block, ws: WavesSettings)(
      assertion: (StateSnapshot, StateSnapshot) => Unit
  ): Unit =
    withDomain(ws) { d =>
      val BlockDiffer.Result(snapshot, _, _, _, detailedSnapshot, _) =
        BlockDiffer
          .fromBlock(d.blockchain, Some(d.lastBlock), block, None, MiningConstraint.Unlimited, block.header.generationSignature)
          .explicitGet()
      assertion(snapshot, detailedSnapshot)
    }

  "BlockDiffer DetailedSnapshot" - {
    "works in case of one genesis transaction" in {
      val genesisBlock: (Address, Block) = {
        val master       = TxHelpers.signer(1)
        val genesisBlock = TestBlock.create(System.currentTimeMillis(), Seq(TxHelpers.genesis(master.toAddress))).block
        (master.toAddress, genesisBlock)
      }

      val (master, b) = genesisBlock
      assertDetailedSnapshot(b, RideV6) { case (snapshot, keyBlockSnapshot) =>
        snapshot.balances((master, Waves)) shouldBe ENOUGH_AMT
        keyBlockSnapshot.balances.get((master, Waves)) shouldBe None
        keyBlockSnapshot.transactions.size shouldBe 1
        keyBlockSnapshot.transactions.head._2.snapshot.balances((master, Waves)) shouldBe ENOUGH_AMT
      }
    }

    "genesis and transfers" - {
      val fee1 = 999999
      val fee2 = 100000

      val gAmount = 30.waves
      val amount1 = 15.waves
      val amount2 = 7.waves

      val a1 = TxHelpers.signer(1)
      val a2 = TxHelpers.signer(2)

      val genesis   = TxHelpers.genesis(a1.toAddress, gAmount)
      val transfer1 = TxHelpers.transfer(a1, a2.toAddress, amount1, fee = fee1, version = TxVersion.V1)
      val transfer2 = TxHelpers.transfer(a2, a1.toAddress, amount2, fee = fee2, version = TxVersion.V1)
      val block     = TestBlock.create(a1, Seq(genesis, transfer1, transfer2))
      val address1  = a1.toAddress
      val address2  = a2.toAddress

      "transaction snapshots are correct" in {
        assertDetailedSnapshot(block.block, RideV6) { case (_, keyBlockSnapshot) =>
          val transactionSnapshots = keyBlockSnapshot.transactions.map(_._2.snapshot).toSeq
          transactionSnapshots(0).balances((address1, Waves)) shouldBe gAmount
          transactionSnapshots(1).balances((address1, Waves)) shouldBe gAmount - amount1 - fee1 + fee1 / 5 * 2
          transactionSnapshots(1).balances((address2, Waves)) shouldBe amount1
          transactionSnapshots(2).balances((address1, Waves)) shouldBe gAmount - amount1 - fee1 + fee1 / 5 * 2 + amount2 + fee2 / 5 * 2
          transactionSnapshots(2).balances((address2, Waves)) shouldBe amount1 - amount2 - fee2
        }
      }

      "miner reward is correct" - {
        "without NG" in {
          assertDetailedSnapshot(block.block, SettingsFromDefaultConfig) { case (_, keyBlockSnapshot) =>
            keyBlockSnapshot.balances((address1, Waves)) shouldBe fee1 + fee2
          }
        }

        "with NG" - {
          "no history — no reward" in {
            assertDetailedSnapshot(block.block, NG) { case (_, keyBlockSnapshot) =>
              keyBlockSnapshot.balances shouldBe empty
            }
          }

          "with history — all fee from last" in {
            val a1 = TxHelpers.signer(1)
            val a2 = TxHelpers.signer(2)

            val amount1 = 2.waves
            val amount2 = 1.waves

            val genesis   = TxHelpers.genesis(a1.toAddress)
            val transfer1 = TxHelpers.transfer(a1, a2.toAddress, amount1, fee = fee1, version = TxVersion.V1)
            val transfer2 = TxHelpers.transfer(a2, a1.toAddress, amount2, fee = fee2, version = TxVersion.V1)

            withDomain(NG) { d =>
              d.appendBlock(genesis)
              d.appendBlock(transfer1)
              val block = TestBlock.create(defaultSigner, Seq(transfer2)).block
              val BlockDiffer.Result(_, _, _, _, detailedSnapshot, _) =
                BlockDiffer
                  .fromBlock(d.blockchain, Some(d.lastBlock), block, None, MiningConstraint.Unlimited, block.header.generationSignature)
                  .explicitGet()
              detailedSnapshot.balances((defaultAddress, Waves)) shouldBe fee1
            }
          }
        }
      }
    }
  }
}
