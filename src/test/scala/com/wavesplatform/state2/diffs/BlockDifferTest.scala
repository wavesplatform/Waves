package com.wavesplatform.state2.diffs

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.BlockGen
import com.wavesplatform.db.WithState
import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.Diff
import com.wavesplatform.state2.reader.SnapshotStateReader
import org.scalatest.{FreeSpecLike, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.{GenesisTransaction, TransactionParser, ValidationError}

class BlockDifferTest extends FreeSpecLike with Matchers with BlockGen with WithState {

  private val TransactionFee = 10

  def randomPrivateKeyAccount(): PrivateKeyAccount = {
    val seed = Array.ofDim[Byte](TransactionParser.KeyLength)
    ThreadLocalRandom.current().nextBytes(seed)
    PrivateKeyAccount(seed)
  }

  private val signerA, signerB = randomPrivateKeyAccount()

  private val testChain: Seq[Block] = {
    val master, recipient = randomPrivateKeyAccount()
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
        assertDiff(testChain.init, TestFunctionalitySettings.Enabled, 1000) { case (_, s) =>
          s.portfolio(signerA).balance shouldBe 40
        }

        assertDiff(testChain, TestFunctionalitySettings.Enabled, 1000) { case (_, s) =>
          s.portfolio(signerB).balance shouldBe 50
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
        assertDiff(testChain, TestFunctionalitySettings.Enabled, 9) { case (_, s) =>
          s.portfolio(signerB).balance shouldBe 44
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
        assertDiff(testChain.init, TestFunctionalitySettings.Enabled, 4) { case (_, s) =>
          s.portfolio(signerA).balance shouldBe 34
        }

        assertDiff(testChain, TestFunctionalitySettings.Enabled, 4) { case (_, s) =>
          s.portfolio(signerB).balance shouldBe 50
        }
      }
    }
  }

  private def assertDiff(blocks: Seq[Block], fs: FunctionalitySettings, ngAtHeight: Int)
                        (assertion: (Diff, SnapshotStateReader) => Unit): Unit = {
    val fp = new FeatureProvider {
      override def activatedFeatures() = Map(BlockchainFeatures.NG.id -> ngAtHeight)

      override def featureVotes(height: Int) = ???

      override def approvedFeatures() = Map(BlockchainFeatures.NG.id -> 0)
    }
    assertDiffEiWithPrev(blocks.init, blocks.last, fp, fs)(assertion)
  }

  private def assertDiffEiWithPrev(preconditions: Seq[Block], block: Block, fp: FeatureProvider, fs: FunctionalitySettings)
                                  (assertion: (Diff, SnapshotStateReader) => Unit): Unit = withStateAndHistory(fs) { state =>

    def differ(s: SnapshotStateReader, prev: Option[Block], b: Block): Either[ValidationError, Diff] =
      BlockDiffer.fromBlock(fs, fp, s, prev, b)

    zipWithPrev(preconditions).foreach { case (prev, b) =>
      state.append(differ(state, prev, b).explicitGet(), b)
    }

    val totalDiff1 = differ(state, preconditions.lastOption, block).explicitGet()
    state.append(totalDiff1, block)
    assertion(totalDiff1, state)
  }

  private def getTwoMinersBlockChain(from: PrivateKeyAccount,
                                     to: PrivateKeyAccount,
                                     numPayments: Int): Seq[Block] = {
    val ts = System.currentTimeMillis() - 100000
    val genesisTx = GenesisTransaction.create(from, Long.MaxValue - 1, ts).right.get

    val paymentTxs = (1 to numPayments).map { i =>
      createWavesTransfer(
        from,
        to,
        amount = 10000,
        TransactionFee,
        timestamp = ts + i * 1000
      ).right.get
    }

    (genesisTx +: paymentTxs).zipWithIndex.map { case (x, i) =>
      val signer = if (i % 2 == 0) signerA else signerB
      TestBlock.create(signer, Seq(x))
    }
  }
}
