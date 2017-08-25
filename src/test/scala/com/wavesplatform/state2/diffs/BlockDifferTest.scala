package com.wavesplatform.state2.diffs

import com.wavesplatform.BlockGen
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.BlockDiff
import org.scalatest.{FreeSpecLike, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.{GenesisTransaction, PaymentTransaction, ValidationError}

class BlockDifferTest extends FreeSpecLike with Matchers with BlockGen {

  private val NumTransactions = 10
  private val TransactionFee = 10

  private val signer1, signer2 = PrivateKeyAccount.random

  private val testChain: Seq[Block] = {
    val master, recipient = PrivateKeyAccount.random
    getTwoMinersBlockChain(master, recipient, NumTransactions)
  }

  "BlockDiffer" - {
    "enableMicroblocksAfterHeight" - {
      "height < enableMicroblocksAfterHeight - a miner should receive 100% of the current block's fee" in {
        val fs = TestFunctionalitySettings.Enabled.copy(
          enableMicroblocksAfterHeight = NumTransactions + 1000
        )

        assertDiff(testChain, fs) { diff =>
          val actualTotalFeeOfSigner1 = diff.right.get.snapshots(signer1)(NumTransactions + 1).balance
          actualTotalFeeOfSigner1 shouldBe 50
        }

        assertDiff(testChain.init, fs) { diff =>
          val actualTotalFeeOfSigner2 = diff.right.get.snapshots(signer2)(NumTransactions).balance
          actualTotalFeeOfSigner2 shouldBe 50
        }
      }

      "height = enableMicroblocksAfterHeight - a miner should receive 40% of the current block's fee only" in {
        val fs = TestFunctionalitySettings.Enabled.copy(
          enableMicroblocksAfterHeight = NumTransactions
        )

        assertDiff(testChain, fs) { diff =>
          val actualTotalFeeOfSigner1 = diff.right.get.snapshots(signer1)(NumTransactions + 1).balance
          actualTotalFeeOfSigner1 shouldBe 44
        }
      }

      "height > enableMicroblocksAfterHeight - a miner should receive 60% of previous block's fee and 40% of the current one" in {
        val fs = TestFunctionalitySettings.Enabled.copy(
          enableMicroblocksAfterHeight = NumTransactions / 2
        )

        assertDiff(testChain, fs) { diff =>
          val actualTotalFeeOfSigner1 = diff.right.get.snapshots(signer1)(NumTransactions + 1).balance
          actualTotalFeeOfSigner1 shouldBe 50
        }

        assertDiff(testChain.init, fs) { diff =>
          val actualTotalFeeOfSigner2 = diff.right.get.snapshots(signer2)(NumTransactions).balance
          actualTotalFeeOfSigner2 shouldBe 44
        }
      }
    }
  }

  private def assertDiff(blocks: Seq[Block], fs: FunctionalitySettings)
                        (assertion: Either[ValidationError, BlockDiff] => Unit): Unit = {
    assertDiffEiWithPrev(blocks.init, blocks.last, fs)(assertion)
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

    val signer1TestBlock = new TestBlock(signer1)
    val signer2TestBlock = new TestBlock(signer2)

    (genesisTx +: paymentTxs).zipWithIndex.map { case (x, i) =>
      val testBlock = if (i % 2 == 0) signer1TestBlock else signer2TestBlock
      testBlock.create(Seq(x))
    }
  }

}
