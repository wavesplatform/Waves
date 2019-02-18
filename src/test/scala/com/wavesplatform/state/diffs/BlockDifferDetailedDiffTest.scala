package com.wavesplatform.state.diffs

import java.util.concurrent.ThreadLocalRandom

import com.wavesplatform.BlockGen
import com.wavesplatform.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.block.Block
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings => TFS}
import com.wavesplatform.state.diffs.BlockDiffer.DetailedDiff
import com.wavesplatform.state.{Blockchain, Diff}
import com.wavesplatform.transaction.GenesisTransaction
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

//import scala.concurrent.duration._

class BlockDifferDetailedDiffTest extends FreeSpec with Matchers with PropertyChecks with BlockGen with WithState {

  private def assertDetailedDiff(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TFS.Enabled)(
      assertion: (Diff, DetailedDiff) => Unit): Unit =
    withStateAndHistory(fs) { state =>
      def differ(blockchain: Blockchain, prevBlock: Option[Block], b: Block) =
        BlockDiffer.fromBlock(fs, blockchain, prevBlock, b, MiningConstraint.Unlimited)

      preconditions.foldLeft[Option[Block]](None) { (prevBlock, curBlock) =>
        val (diff, fees, _, _) = differ(state, prevBlock, curBlock).explicitGet()
        state.append(diff, fees, curBlock)
        Some(curBlock)
      }

      val (diff, _, _, detailedDiff) = differ(state, preconditions.lastOption, block).explicitGet()
      assertion(diff, detailedDiff)
    }
//
//  val setup: Gen[(GenesisTransaction, LeaseTransaction, LeaseTransaction)] = for {
//    master    <- accountGen
//    recipient <- accountGen suchThat (_ != master)
//    forward   <- accountGen suchThat (!Set(master, recipient).contains(_))
//    ts        <- positiveIntGen
//    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
//    (lease, _)        <- leaseAndCancelGeneratorP(master, recipient, master)
//    (leaseForward, _) <- leaseAndCancelGeneratorP(recipient, forward, recipient)
//  } yield (genesis, lease, leaseForward)

  "BlockDiffer DetailedDiff" - {
    "works in case of one genesis transaction" in {
      val genesisBlockGen: Gen[(Address, Block)] = for {
        master <- accountGen
        ts     <- positiveIntGen
        genesisBlock = TestBlock
          .create(ts, Seq(GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()))
//          b1 = TestBlock
//            .create(
//              ts + 10,
//              genesisBlock.uniqueId,
//              Seq(
//                createTransfer(master, recipient.toAddress, ts + 1),
//                createTransfer(master, recipient.toAddress, ts + 2),
//                createTransfer(recipient, master.toAddress, ts + 3),
//                createTransfer(master, recipient.toAddress, ts + 4),
//                createTransfer(master, recipient.toAddress, ts + 5)
//              )
//            )
      } yield (PublicKeyAccount.toAddress(master), genesisBlock)

      forAll(genesisBlockGen) {
        case (master, b) =>
          assertDetailedDiff(Seq.empty, b) {
            case (diff, detailedDiff) =>
              diff.portfolios(master).balance shouldBe ENOUGH_AMT
              detailedDiff._1.portfolios(master).balance shouldBe 0
              detailedDiff._2.head.portfolios(master).balance shouldBe 0
          }
      }

    }
  }
//  private val TransactionFee = 10
//
////  def randomPrivateKeyAccount(): PrivateKeyAccount = {
////    val seed = Array.ofDim[Byte](KeyLength)
////    ThreadLocalRandom.current().nextBytes(seed)
////    PrivateKeyAccount(seed)
////  }
//
//  private val signerA, signerB = randomPrivateKeyAccount()
//
//  private val testChain: Seq[Block] = {
//    val master, recipient = randomPrivateKeyAccount()
//    getTwoMinersBlockChain(master, recipient, 9)
//  }
//
//  "BlockDiffer" - {
//    "enableMicroblocksAfterHeight" - {
//      /*
//      | N | fee | signer | A receive | A balance | B receive | B balance |
//      |--:|:---:|:------:|----------:|----------:|----------:|-----------|
//      |1  |0    |A       |0          |0          |0          |0          | <- genesis
//      |2  |10   |B       |0          |0          |10         |+10        |
//      |3  |10   |A       |10         |+10        |0          |0          |
//      |4  |10   |B       |0          |10         |+10        |10+10=20   |
//      |5  |10   |A       |10         |10+10=20   |0          |20         |
//      |6  |10   |B       |0          |20         |+10        |20+10=30   |
//      |7  |10   |A       |10         |20+10=30   |0          |30         |
//      |8  |10   |B       |0          |30         |+10        |30+10=40   |
//      |9  |10   |A       |10         |30+10=40   |0          |40         | <- 1st check
//      |10 |10   |B       |0          |40         |+10        |40+10=50   | <- 2nd check
//       */
//      "height < enableMicroblocksAfterHeight - a miner should receive 100% of the current block's fee" in {
//        assertDiff(testChain.init, 1000) {
//          case (_, s) =>
//            s.portfolio(signerA).balance shouldBe 40
//        }
//
//        assertDiff(testChain, 1000) {
//          case (_, s) =>
//            s.portfolio(signerB).balance shouldBe 50
//        }
//      }
//
//      /*
//      | N | fee | signer | A receive | A balance | B receive | B balance |
//      |--:|:---:|:------:|----------:|----------:|----------:|-----------|
//      |1  |0    |A       |0          |0          |0          |0          | <- genesis
//      |2  |10   |B       |0          |0          |10         |+10        |
//      |3  |10   |A       |10         |+10        |0          |0          |
//      |4  |10   |B       |0          |10         |+10        |10+10=20   |
//      |5  |10   |A       |10         |10+10=20   |0          |20         |
//      |6  |10   |B       |0          |20         |+10        |20+10=30   |
//      |7  |10   |A       |10         |20+10=30   |0          |30         |
//      |8  |10   |B       |0          |30         |+10        |30+10=40   |
//      |9  |10   |A       |10         |30+10=40   |0          |40         |
//      |-------------------------- Enable NG -----------------------------|
//      |10 |10   |B       |0          |40         |+4         |40+4=44    | <- check
//       */
//      "height = enableMicroblocksAfterHeight - a miner should receive 40% of the current block's fee only" in {
//        assertDiff(testChain, 9) {
//          case (_, s) =>
//            s.portfolio(signerB).balance shouldBe 44
//        }
//      }
//
//      /*
//      | N | fee | signer | A receive | A balance | B receive | B balance |
//      |--:|:---:|:------:|----------:|----------:|----------:|-----------|
//      |1  |0    |A       |0          |0          |0          |0          | <- genesis
//      |2  |10   |B       |0          |0          |10         |+10        |
//      |3  |10   |A       |10         |+10        |0          |0          |
//      |4  |10   |B       |0          |10         |+10        |10+10=20   |
//      |-------------------------- Enable NG -----------------------------|
//      |5  |10   |A       |4          |10+4=14    |0          |20         |
//      |6  |10   |B       |0          |14         |+4+6=10    |20+10=30   |
//      |7  |10   |A       |4+6=10     |14+10=24   |0          |30         |
//      |8  |10   |B       |0          |24         |+4+6=10    |30+10=40   |
//      |9  |10   |A       |4+6=10     |24+10=34   |0          |40         | <- 1st check
//      |10 |10   |B       |0          |34         |+4+6=10    |40+10=50   | <- 2nd check
//       */
//      "height > enableMicroblocksAfterHeight - a miner should receive 60% of previous block's fee and 40% of the current one" in {
//        assertDiff(testChain.init, 4) {
//          case (_, s) =>
//            s.portfolio(signerA).balance shouldBe 34
//        }
//
//        assertDiff(testChain, 4) {
//          case (_, s) =>
//            s.portfolio(signerB).balance shouldBe 50
//        }
//      }
//    }
//  }
//
//  private def assertDiff(blocks: Seq[Block], ngAtHeight: Int)(assertion: (Diff, Blockchain) => Unit): Unit = {
//    val fs = FunctionalitySettings(
//      featureCheckBlocksPeriod = ngAtHeight / 2,
//      blocksForFeatureActivation = 1,
//      allowTemporaryNegativeUntil = 0L,
//      generationBalanceDepthFrom50To1000AfterHeight = 0,
//      minimalGeneratingBalanceAfter = 0L,
//      allowTransactionsFromFutureUntil = Long.MaxValue,
//      allowUnissuedAssetsUntil = 0L,
//      allowInvalidReissueInSameBlockUntilTimestamp = 0L,
//      allowMultipleLeaseCancelTransactionUntilTimestamp = 0L,
//      resetEffectiveBalancesAtHeight = 0,
//      blockVersion3AfterHeight = 0,
//      preActivatedFeatures = Map[Short, Int]((2, ngAtHeight)),
//      doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
//      maxTransactionTimeBackOffset = 120.minutes,
//      maxTransactionTimeForwardOffset = 90.minutes
//    )
//    assertNgDiffState(blocks.init, blocks.last, fs)(assertion)
//  }
//
//  private def getTwoMinersBlockChain(from: PrivateKeyAccount, to: PrivateKeyAccount, numPayments: Int): Seq[Block] = {
//    val ts                   = System.currentTimeMillis() - 100000
//    val genesisTx            = GenesisTransaction.create(from, Long.MaxValue - 1, ts).explicitGet()
//    val features: Set[Short] = Set[Short](2)
//
//    val paymentTxs = (1 to numPayments).map { i =>
//      createWavesTransfer(
//        from,
//        to,
//        amount = 10000,
//        TransactionFee,
//        timestamp = ts + i * 1000
//      ).explicitGet()
//    }
//
//    (genesisTx +: paymentTxs).zipWithIndex.map {
//      case (x, i) =>
//        val signer = if (i % 2 == 0) signerA else signerB
//        TestBlock.create(signer, Seq(x), features)
//    }
//  }
}
