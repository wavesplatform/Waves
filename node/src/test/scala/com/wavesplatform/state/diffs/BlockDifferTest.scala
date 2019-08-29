package com.wavesplatform.state.diffs

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.BlockGen
import com.wavesplatform.account.KeyPair
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.crypto._
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.settings.{BlockRewardSettings, FunctionalitySettings}
import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.GenesisTransaction
import org.scalatest.{FreeSpecLike, Matchers}

class BlockDifferTest extends FreeSpecLike with Matchers with BlockGen with WithState {

  private val TransactionFee = 10

  def randomKeyPair(): KeyPair = {
    val seed = Array.ofDim[Byte](KeyLength)
    ThreadLocalRandom.current().nextBytes(seed)
    KeyPair(seed)
  }

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
        assertDiff(testChain.init, 1000) {
          case (_, s) =>
            s.balance(signerA) shouldBe 40
        }

        assertDiff(testChain, 1000) {
          case (_, s) =>
            s.balance(signerB) shouldBe 50
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
        assertDiff(testChain, 9) {
          case (_, s) =>
            s.balance(signerB) shouldBe 44
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
        assertDiff(testChain.init, 4) {
          case (_, s) =>
            s.balance(signerA) shouldBe 34
        }

        assertDiff(testChain, 4) {
          case (_, s) =>
            s.balance(signerB) shouldBe 50
        }
      }

      /*
      | N | fee | reward | signer | A receive | A balance | B receive | B balance |
      |--:|:---:|:------:|:------:|----------:|----------:|----------:|-----------|
      |1  |0    |0       |A       |0          |0          |0          |0          | <- genesis
      |2  |10   |0       |B       |0          |0          |10         |+10        |
      |3  |10   |0       |A       |10         |+10        |0          |0          |
      |4  |10   |0       |B       |0          |10         |+10        |10+10=20   |
      |---------------------- Enable NG and BlockReward --------------------------|
      |5  |10   |0       |A       |4          |10+4=14    |0          |20         |
      |6  |10   |6       |B       |0          |14         |+4+6+6=10  |20+16=36   |
      |7  |10   |6       |A       |4+6+6=16   |14+16=30   |0          |36         |
      |8  |10   |6       |B       |0          |30         |+4+6+6=16  |36+16=52   |
      |9  |10   |6       |A       |4+6+6=16   |30+16=46   |0          |52         | <- 1st check
      |10 |10   |6       |B       |0          |46         |+4+6+6=16  |52+16=68   | <- 2nd check
       */
      "height > BlockReward activation - a miner should receive 60% of previous block's fee and 40% of the current one + reward" in {
        val rewardSettings = BlockRewardSettings(0, 6, 25000, 10, 10)
        assertDiff(testChain.init, 4, rewardSettings) {
          case (_, s) =>
            s.balance(signerA) shouldBe 46
        }

        assertDiff(testChain, 4, rewardSettings) {
          case (_, s) =>
            s.balance(signerB) shouldBe 68
        }
      }
    }
  }

  private def assertDiff(blocks: Seq[Block], ngAtHeight: Int)(assertion: (Diff, Blockchain) => Unit): Unit =
    assertDiff(blocks, ngAtHeight / 2, Map((BlockchainFeatures.NG.id, ngAtHeight)), BlockRewardSettings(0, 0, 1, 1, 1))(assertion)

  private def assertDiff(blocks: Seq[Block], rewardAtHeight: Int, settings: BlockRewardSettings)(assertion: (Diff, Blockchain) => Unit): Unit = {
    val features = Map((BlockchainFeatures.NG.id, rewardAtHeight), (BlockchainFeatures.BlockReward.id, rewardAtHeight))
    assertDiff(blocks, rewardAtHeight / 2, features, settings)(assertion)
  }

  private def assertDiff(
      blocks: Seq[Block],
      featuresCheckBlocksPeriod: Int,
      preActivatedFeatures: Map[Short, Int],
      blockRewardSettings: BlockRewardSettings
  )(assertion: (Diff, Blockchain) => Unit): Unit = {
    val fs = FunctionalitySettings(
      featureCheckBlocksPeriod = featuresCheckBlocksPeriod,
      blocksForFeatureActivation = 1,
      preActivatedFeatures = preActivatedFeatures,
      doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
      blockRewardSettings = blockRewardSettings
    )
    assertNgDiffState(blocks.init, blocks.last, fs)(assertion)
  }

  private def getTwoMinersBlockChain(from: KeyPair, to: KeyPair, numPayments: Int): Seq[Block] = {
    val ts                   = System.currentTimeMillis() - 100000
    val genesisTx            = GenesisTransaction.create(from, Long.MaxValue - 1, ts).explicitGet()
    val features: Set[Short] = Set[Short](2)

    val paymentTxs = (1 to numPayments).map { i =>
      createWavesTransfer(
        from,
        to,
        amount = 10000,
        TransactionFee,
        timestamp = ts + i * 1000
      ).explicitGet()
    }

    (genesisTx +: paymentTxs).zipWithIndex.map {
      case (x, i) =>
        val signer = if (i % 2 == 0) signerA else signerB
        TestBlock.create(signer, Seq(x), features)
    }
  }
}
