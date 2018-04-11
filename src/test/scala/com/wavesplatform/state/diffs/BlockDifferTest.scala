package com.wavesplatform.state.diffs

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.BlockGen
import com.wavesplatform.db.WithState
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state.reader.LeaseDetails
import com.wavesplatform.state.{AccountDataInfo, AssetDescription, BalanceSnapshot, Blockchain, ByteStr, DataEntry, Diff, Portfolio, VolumeAndFee}
import org.scalatest.{FreeSpecLike, Matchers}
import scorex.account.{Address, Alias, PrivateKeyAccount}
import scorex.block.{Block, BlockHeader}
import scorex.crypto.signatures.Curve25519.KeyLength
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.Transaction.Type
import scorex.transaction.lease.LeaseTransaction
import scorex.transaction.smart.Script
import scorex.transaction.{GenesisTransaction, Transaction, ValidationError}

class BlockDifferTest extends FreeSpecLike with Matchers with BlockGen with WithState {

  private val TransactionFee = 10

  def randomPrivateKeyAccount(): PrivateKeyAccount = {
    val seed = Array.ofDim[Byte](KeyLength)
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
        assertDiff(testChain.init, TestFunctionalitySettings.Enabled, 1000) {
          case (_, s) =>
            s.portfolio(signerA).balance shouldBe 40
        }

        assertDiff(testChain, TestFunctionalitySettings.Enabled, 1000) {
          case (_, s) =>
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
        assertDiff(testChain, TestFunctionalitySettings.Enabled, 9) {
          case (_, s) =>
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
        assertDiff(testChain.init, TestFunctionalitySettings.Enabled, 4) {
          case (_, s) =>
            s.portfolio(signerA).balance shouldBe 34
        }

        assertDiff(testChain, TestFunctionalitySettings.Enabled, 4) {
          case (_, s) =>
            s.portfolio(signerB).balance shouldBe 50
        }
      }
    }
  }

  //TODO: Use functionality settings and activate NG in preactivated features

  private def assertDiff(blocks: Seq[Block], fs: FunctionalitySettings, ngAtHeight: Int)(assertion: (Diff, Blockchain) => Unit): Unit = {
    val blockchain = new Blockchain {
      override def height: Int = ???

      override def score: BigInt = ???

      override def scoreOf(blockId: ByteStr): Option[BigInt] = ???

      override def blockHeaderAndSize(height: Int): Option[(BlockHeader, Int)] = ???

      override def blockHeaderAndSize(blockId: ByteStr): Option[(BlockHeader, Int)] = ???

      override def lastBlock: Option[Block] = ???

      override def blockBytes(height: Int): Option[Array[Byte]] = ???

      override def blockBytes(blockId: ByteStr): Option[Array[Byte]] = ???

      override def heightOf(blockId: ByteStr): Option[Int] = ???

      /** Returns the most recent block IDs, starting from the most recent  one */
      override def lastBlockIds(howMany: Int): Seq[ByteStr] = ???

      /** Returns a chain of blocks starting with the block with the given ID (from oldest to newest) */
      override def blockIdsAfter(parentSignature: ByteStr, howMany: Int): Option[Seq[ByteStr]] = ???

      override def parent(block: Block, back: Int): Option[Block] = ???

      /** Features related */
      override def activatedFeatures() = Map(BlockchainFeatures.NG.id -> ngAtHeight)

      override def featureVotes(height: Int) = ???

      override def approvedFeatures() = Map(BlockchainFeatures.NG.id -> 0)

      override def portfolio(a: Address): Portfolio = ???

      override def transactionInfo(id: ByteStr): Option[(Int, Transaction)] = ???

      override def addressTransactions(address: Address, types: Set[Type], count: Int, from: Int): Seq[(Int, Transaction)] = ???

      override def containsTransaction(id: ByteStr): Boolean = ???

      override def assetDescription(id: ByteStr): Option[AssetDescription] = ???

      override def resolveAlias(a: Alias): Option[Address] = ???

      override def leaseDetails(leaseId: ByteStr): Option[LeaseDetails] = ???

      override def filledVolumeAndFee(orderId: ByteStr): VolumeAndFee = ???

      /** Retrieves Waves balance snapshot in the [from, to] range (inclusive) */
      override def balanceSnapshots(address: Address, from: Int, to: Int): Seq[BalanceSnapshot] = ???

      override def accountScript(address: Address): Option[Script] = ???

      override def accountData(acc: Address): AccountDataInfo = ???

      override def accountData(acc: Address, key: String): Option[DataEntry[_]] = ???

      override def assetDistribution(height: Int, assetId: ByteStr): Map[Address, Long] = ???

      override def wavesDistribution(height: Int): Map[Address, Long] = ???

      override def allActiveLeases: Set[LeaseTransaction] = ???

      /** Builds a new portfolio map by applying a partial function to all portfolios on which the function is defined.
        *
        * @note Portfolios passed to `pf` only contain Waves and Leasing balances to improve performance */
      override def collectLposPortfolios[A](pf: PartialFunction[(Address, Portfolio), A]): Map[Address, A] = ???
      override def append(diff: Diff, block: Block): Unit                                                  = ???
      override def rollbackTo(targetBlockId: ByteStr): Seq[Block]                                          = ???
    }
    assertDiffEiWithPrev(blocks.init, blocks.last, blockchain, fs)(assertion)
  }

  private def assertDiffEiWithPrev(preconditions: Seq[Block], block: Block, blockchain: Blockchain, fs: FunctionalitySettings)(
      assertion: (Diff, Blockchain) => Unit): Unit = withStateAndHistory(fs) { bc =>
    def differ(prev: Option[Block], b: Block): Either[ValidationError, Diff] =
      BlockDiffer.fromBlock(fs, blockchain, prev, b)

    zipWithPrev(preconditions).foreach {
      case (prev, b) =>
        bc.append(differ(prev, b).explicitGet(), b)
    }

    val totalDiff1 = differ(preconditions.lastOption, block).explicitGet()
    bc.append(totalDiff1, block)
    assertion(totalDiff1, bc)
  }

  private def getTwoMinersBlockChain(from: PrivateKeyAccount, to: PrivateKeyAccount, numPayments: Int): Seq[Block] = {
    val ts        = System.currentTimeMillis() - 100000
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

    (genesisTx +: paymentTxs).zipWithIndex.map {
      case (x, i) =>
        val signer = if (i % 2 == 0) signerA else signerB
        TestBlock.create(signer, Seq(x))
    }
  }
}
