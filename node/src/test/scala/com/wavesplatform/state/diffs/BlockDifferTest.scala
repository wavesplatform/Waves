package com.wavesplatform.state.diffs

import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto.DigestLength
import com.wavesplatform.db.WithDomain
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{FunctionalitySettings, GenesisSettings, GenesisTransactionSettings}
import com.wavesplatform.state.diffs.BlockDiffer.InvalidStateHash
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.state.{Blockchain, Diff, TxStateSnapshotHashBuilder}
import com.wavesplatform.test.*
import com.wavesplatform.test.node.*
import com.wavesplatform.transaction.{GenesisTransaction, TxHelpers, TxVersion}

import scala.concurrent.duration.*

class BlockDifferTest extends FreeSpec with WithDomain {
  private val TransactionFee = 10

  private val signerA, signerB = randomKeyPair()

  private val testChain: Seq[Block] = {
    val master, recipient = randomKeyPair()
    getTwoMinersBlockChain(master, recipient, 9)
  }

  "BlockDiffer" - {
    "enableMicroblocksAfterHeight" - {
      /*
      | N | fee | signer | A receive | A balance | B receive | B balance |
      |--:|:---:|:------:|----------:|----------:|----------:|-----------|
      |1  |0    |A       |0          |0          |0          |0          | <- genesis
      |2  |10   |B       |0          |0          |10         |+10        |
      |3  |10   |A       |10         |+10        |0          |0          |
      |4  |10   |B       |0          |10         |+10        |10+10=20   |
      |5  |10   |A       |10         |10+10=20   |0          |20         |
      |6  |10   |B       |0          |20         |+10        |20+10=30   |
      |7  |10   |A       |10         |20+10=30   |0          |30         |
      |8  |10   |B       |0          |30         |+10        |30+10=40   |
      |9  |10   |A       |10         |30+10=40   |0          |40         | <- 1st check
      |10 |10   |B       |0          |40         |+10        |40+10=50   | <- 2nd check
       */
      "height < enableMicroblocksAfterHeight - a miner should receive 100% of the current block's fee" in {
        assertDiff(testChain.init, 1000) { case (_, s) =>
          s.balance(signerA.toAddress) shouldBe 40
        }

        assertDiff(testChain, 1000) { case (_, s) =>
          s.balance(signerB.toAddress) shouldBe 50
        }
      }

      /*
      | N | fee | signer | A receive | A balance | B receive | B balance |
      |--:|:---:|:------:|----------:|----------:|----------:|-----------|
      |1  |0    |A       |0          |0          |0          |0          | <- genesis
      |2  |10   |B       |0          |0          |10         |+10        |
      |3  |10   |A       |10         |+10        |0          |0          |
      |4  |10   |B       |0          |10         |+10        |10+10=20   |
      |5  |10   |A       |10         |10+10=20   |0          |20         |
      |6  |10   |B       |0          |20         |+10        |20+10=30   |
      |7  |10   |A       |10         |20+10=30   |0          |30         |
      |8  |10   |B       |0          |30         |+10        |30+10=40   |
      |9  |10   |A       |10         |30+10=40   |0          |40         |
      |-------------------------- Enable NG -----------------------------|
      |10 |10   |B       |0          |40         |+4         |40+4=44    | <- check
       */
      "height = enableMicroblocksAfterHeight - a miner should receive 40% of the current block's fee only" in {
        assertDiff(testChain, 9) { case (_, s) =>
          s.balance(signerB.toAddress) shouldBe 44
        }
      }

      /*
      | N | fee | signer | A receive | A balance | B receive | B balance |
      |--:|:---:|:------:|----------:|----------:|----------:|-----------|
      |1  |0    |A       |0          |0          |0          |0          | <- genesis
      |2  |10   |B       |0          |0          |10         |+10        |
      |3  |10   |A       |10         |+10        |0          |0          |
      |4  |10   |B       |0          |10         |+10        |10+10=20   |
      |-------------------------- Enable NG -----------------------------|
      |5  |10   |A       |4          |10+4=14    |0          |20         |
      |6  |10   |B       |0          |14         |+4+6=10    |20+10=30   |
      |7  |10   |A       |4+6=10     |14+10=24   |0          |30         |
      |8  |10   |B       |0          |24         |+4+6=10    |30+10=40   |
      |9  |10   |A       |4+6=10     |24+10=34   |0          |40         | <- 1st check
      |10 |10   |B       |0          |34         |+4+6=10    |40+10=50   | <- 2nd check
       */
      "height > enableMicroblocksAfterHeight - a miner should receive 60% of previous block's fee and 40% of the current one" in {
        assertDiff(testChain.init, 4) { case (_, s) =>
          s.balance(signerA.toAddress) shouldBe 34
        }

        assertDiff(testChain, 4) { case (_, s) =>
          s.balance(signerB.toAddress) shouldBe 50
        }
      }
    }

    "correctly computes state hash" - {
      "genesis block" in {
        val txs = (1 to 10).map(idx => TxHelpers.genesis(TxHelpers.address(idx), 100.waves)) ++
          (1 to 5).map(idx => TxHelpers.genesis(TxHelpers.address(idx), 1.waves))
        withDomain(DomainPresets.TransactionStateSnapshot) { d =>
          val block = createGenesisWithStateHash(txs, txStateSnapshotActivated = true)

          block.header.stateHash shouldBe defined
          BlockDiffer
            .fromBlock(d.blockchain, None, block, MiningConstraint.Unlimited, block.header.generationSignature) should beRight
        }

        withDomain(DomainPresets.RideV6) { d =>
          val block = createGenesisWithStateHash(txs, txStateSnapshotActivated = false)

          block.header.stateHash shouldBe None
          BlockDiffer
            .fromBlock(d.blockchain, None, block, MiningConstraint.Unlimited, block.header.generationSignature) should beRight
        }
      }

      "arbitrary block/microblock" in
        withDomain(DomainPresets.TransactionStateSnapshot) { d =>
          val genesis = createGenesisWithStateHash(Seq(TxHelpers.genesis(TxHelpers.address(1))), txStateSnapshotActivated = true)
          d.appendBlock(genesis)

          val txs = (1 to 10).map(idx => TxHelpers.transfer(TxHelpers.signer(idx), TxHelpers.address(idx + 1), (100 - idx).waves))

          val blockTs  = txs.map(_.timestamp).max
          val txDiffer = TransactionDiffer(d.blockchain.lastBlockTimestamp, blockTs) _
          val blockStateHash = txs
            .foldLeft(genesis.header.stateHash.get -> Diff.empty) { case ((prevStateHash, accDiff), tx) =>
              val blockchain = CompositeBlockchain(d.blockchain, accDiff)
              val txDiff     = txDiffer(blockchain, tx).resultE.explicitGet()
              val stateHash  = TxStateSnapshotHashBuilder.createHashFromTxDiff(blockchain, txDiff).createHash(prevStateHash)
              (stateHash, accDiff.combineF(txDiff).explicitGet())
            }
            ._1

          val correctBlock =
            TestBlock.create(blockTs, genesis.id(), txs, version = Block.ProtoBlockVersion, stateHash = Some(blockStateHash))
          BlockDiffer
            .fromBlock(d.blockchain, Some(genesis), correctBlock, MiningConstraint.Unlimited, correctBlock.header.generationSignature) should beRight

          val incorrectBlock =
            TestBlock.create(blockTs, genesis.id(), txs, version = Block.ProtoBlockVersion, stateHash = Some(ByteStr.fill(DigestLength)(1)))
          BlockDiffer.fromBlock(
            d.blockchain,
            Some(genesis),
            incorrectBlock,
            MiningConstraint.Unlimited,
            incorrectBlock.header.generationSignature
          ) shouldBe Left(InvalidStateHash)

          val correctMicroblock = d.createMicroBlock(Some(blockStateHash), txs*)
          BlockDiffer.fromMicroBlock(
            d.blockchain,
            d.blockchain.lastBlockTimestamp,
            genesis.header.stateHash,
            correctMicroblock,
            MiningConstraint.Unlimited
          ) should beRight

          val incorrectMicroblock = d.createMicroBlock(Some(ByteStr.fill(DigestLength)(1)), txs*)
          BlockDiffer.fromMicroBlock(
            d.blockchain,
            d.blockchain.lastBlockTimestamp,
            genesis.header.stateHash,
            incorrectMicroblock,
            MiningConstraint.Unlimited
          ) shouldBe Left(InvalidStateHash)
        }
    }
  }

  private def assertDiff(blocks: Seq[Block], ngAtHeight: Int)(assertion: (Diff, Blockchain) => Unit): Unit = {
    val fs = FunctionalitySettings(
      featureCheckBlocksPeriod = ngAtHeight / 2,
      blocksForFeatureActivation = 1,
      preActivatedFeatures = Map[Short, Int]((2, ngAtHeight)),
      doubleFeaturesPeriodsAfterHeight = Int.MaxValue
    )
    assertNgDiffState(blocks.init, blocks.last, fs)(assertion)
  }

  private def getTwoMinersBlockChain(from: KeyPair, to: KeyPair, numPayments: Int): Seq[Block] = {
    val genesisTx            = TxHelpers.genesis(from.toAddress, Long.MaxValue - 1)
    val features: Seq[Short] = Seq[Short](2)

    val paymentTxs = (1 to numPayments).map { i =>
      TxHelpers.transfer(from, to.toAddress, 10000, fee = TransactionFee, version = TxVersion.V1)
    }

    (genesisTx +: paymentTxs).zipWithIndex.map { case (x, i) =>
      val signer = if (i % 2 == 0) signerA else signerB
      TestBlock.create(signer, Seq(x), features)
    }
  }

  def createGenesisWithStateHash(txs: Seq[GenesisTransaction], txStateSnapshotActivated: Boolean): Block = {
    val timestamp = txs.map(_.timestamp).max
    val genesisSettings = GenesisSettings(
      timestamp,
      timestamp,
      txs.map(_.amount.value).sum,
      None,
      txs.map { tx =>
        GenesisTransactionSettings(tx.recipient.toString, tx.amount.value)
      },
      2L,
      60.seconds
    )

    Block.genesis(genesisSettings, rideV6Activated = true, txStateSnapshotActivated).explicitGet()
  }
}
