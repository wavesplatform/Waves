package com.wavesplatform.state2.diffs

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.BlockGen
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.BlockDiff
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import org.scalatest.{FreeSpecLike, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.{GenesisTransaction, PaymentTransaction, TransactionParser, ValidationError}

class BlockDifferTest extends FreeSpecLike with Matchers with BlockGen {

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
        assertDiff(testChain.init, TestFunctionalitySettings.Enabled, 1000) { case (diff, s) =>
          diff.snapshots(signerA)(9).balance shouldBe 40
          s.balance(signerA) shouldBe 40
        }

        assertDiff(testChain, TestFunctionalitySettings.Enabled, 1000) { case (diff, s) =>
          diff.snapshots(signerB)(10).balance shouldBe 50
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
        assertDiff(testChain, TestFunctionalitySettings.Enabled, 9) { case (diff, s) =>
          diff.snapshots(signerB)(10).balance shouldBe 44
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
        assertDiff(testChain.init, TestFunctionalitySettings.Enabled, 4) { case (diff, s) =>
          diff.snapshots(signerA)(9).balance shouldBe 34
          s.balance(signerA) shouldBe 34
        }

        assertDiff(testChain, TestFunctionalitySettings.Enabled, 4) { case (diff, s) =>
          diff.snapshots(signerB)(10).balance shouldBe 50
          s.balance(signerB) shouldBe 50
        }
      }
    }
  }

  private def assertDiff(blocks: Seq[Block], fs: FunctionalitySettings, ngAtHeight: Int)
                        (assertion: (BlockDiff, StateReader) => Unit): Unit = {
    val fp = new FeatureProvider {
      override def featureActivationHeight(feature: Short): Option[Int] = feature match {
        case BlockchainFeatures.NG.id => Some(ngAtHeight)
        case _ => None
      }

      override def featureStatus(feature: Short): BlockchainFeatureStatus = feature match {
        case BlockchainFeatures.NG.id => BlockchainFeatureStatus.Activated
        case _ => BlockchainFeatureStatus.Undefined
      }

      override def featureVotesCountWithinActivationWindow(height: Int): Map[Short, Int] = ???
    }
    assertDiffEiWithPrev(blocks.init, blocks.last,fp, fs)(assertion)
  }

  private def assertDiffEiWithPrev(preconditions: Seq[Block], block: Block, fp: FeatureProvider, fs: FunctionalitySettings)(assertion: (BlockDiff, StateReader) => Unit): Unit = {
    val state = newState()

    val differ: (StateReader, (Option[Block], Block)) => Either[ValidationError, BlockDiff] = {
      case (s, (prev, b)) => BlockDiffer.fromBlock(fs, fp, s, prev, b)
    }
    zipWithPrev(preconditions).foreach { wp =>
      val preconditionDiff = differ(state, wp).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }

    val totalDiff1 = differ(state, (preconditions.lastOption, block)).explicitGet()
    state.applyBlockDiff(totalDiff1)
    assertion(totalDiff1, state)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(fs, fp, newState(), None)(preconditions)
    val compositeState = new CompositeStateReader(newState(), preconditionDiff)
    val totalDiff2 = differ(compositeState, (preconditions.lastOption, block)).explicitGet()
    assertion(totalDiff2, CompositeStateReader.composite(compositeState, () => totalDiff2))

    assert(totalDiff1 == totalDiff2)
  }

  private def getTwoMinersBlockChain(from: PrivateKeyAccount,
                                     to: PrivateKeyAccount,
                                     numPayments: Int): Seq[Block] = {
    val ts = System.currentTimeMillis() - 100000
    val genesisTx = GenesisTransaction.create(from, Long.MaxValue - 1, ts).right.get

    val paymentTxs = (1 to numPayments).map { i =>
      PaymentTransaction.create(
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
