package com.wavesplatform.state2.diffs

import com.wavesplatform.BlockGen
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{FreeSpecLike, Matchers}
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.lagonaki.mocks.TestBlock
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.{GenesisTransaction, PaymentTransaction}

class BlockDifferTest extends FreeSpecLike with PropertyChecks with GeneratorDrivenPropertyChecks with Matchers with BlockGen {

  private val NumTransactions = 10
  private val TransactionFee = 10

  private def blockChainGen(from: PrivateKeyAccount,
                            to: PrivateKeyAccount,
                            numPayments: Int): Gen[Seq[Block]] = timestampGen.map { ts =>
    val genesisTx = GenesisTransaction.create(from, Long.MaxValue - 1, ts).right.get

    val paymentTxs = (1 to numPayments).map { i =>
      PaymentTransaction.create(from, to, 10000, TransactionFee, ts + i * 1000).right.get
    }

    (genesisTx +: paymentTxs).map { x =>
      TestBlock.create(Seq(x))
    }
  }

  private val testSeq: Gen[Seq[Block]] = for {
    master <- accountGen
    recipient <- accountGen // otherAccountGen(candidate = master)
    chain <- blockChainGen(master, recipient, NumTransactions)
  } yield chain

  private val xs = testSeq.sample.get

  "height > enableMicroblocksAfterHeight - should receive 60% of previous block's fee and 40% of the current one" in {
    val fs = TestFunctionalitySettings.Enabled.copy(
      enableMicroblocksAfterHeight = NumTransactions / 2
    )

    assertDiffEi(xs.init, xs.last, fs) { diff =>
      val actualTotalFee = diff.right.get.snapshots(TestBlock.defaultSigner)(NumTransactions + 1).balance
      actualTotalFee shouldBe 94
    }
  }

}
