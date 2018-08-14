package com.wavesplatform.state

import cats.Monoid
import com.wavesplatform.block.Block
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings => TFS}
import com.wavesplatform.transaction.{Transaction, ValidationError}
import org.scalatest.Matchers

package object diffs extends WithState with Matchers {
  val ENOUGH_AMT: Long = Long.MaxValue / 3

  def assertDiffEi(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TFS.Enabled)(
      assertion: Either[ValidationError, Diff] => Unit): Unit = withStateAndHistory(fs) { state =>
    def differ(blockchain: Blockchain, b: Block) = BlockDiffer.fromBlock(fs, blockchain, None, b, MiningConstraint.Unlimited).map(_._1)

    preconditions.foreach { precondition =>
      val preconditionDiffEI = differ(state, precondition)
      val preconditionDiff   = preconditionDiffEI.explicitGet()
      state.append(preconditionDiff, precondition)
    }
    val totalDiff1 = differ(state, block)
    assertion(totalDiff1)
  }

  def assertDiffAndState(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TFS.Enabled)(
      assertion: (Diff, Blockchain) => Unit): Unit = withStateAndHistory(fs) { state =>
    def differ(blockchain: Blockchain, b: Block) = BlockDiffer.fromBlock(fs, blockchain, None, b, MiningConstraint.Unlimited).map(_._1)

    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, precondition).explicitGet()
      state.append(preconditionDiff, precondition)
    }
    val totalDiff1 = differ(state, block).explicitGet()
    state.append(totalDiff1, block)
    assertion(totalDiff1, state)
  }

  def assertDiffAndState(fs: FunctionalitySettings)(test: (Seq[Transaction] => Either[ValidationError, Unit]) => Unit): Unit =
    withStateAndHistory(fs) { state =>
      def differ(blockchain: Blockchain, b: Block) = BlockDiffer.fromBlock(fs, blockchain, None, b, MiningConstraint.Unlimited).map(_._1)

      test(txs => {
        val block = TestBlock.create(txs)
        differ(state, block).map(diff => state.append(diff, block))
      })
    }

  def assertBalanceInvariant(diff: Diff): Unit = {
    val portfolioDiff = Monoid.combineAll(diff.portfolios.values)
    portfolioDiff.balance shouldBe 0
    portfolioDiff.effectiveBalance shouldBe 0
    portfolioDiff.assets.values.foreach(_ shouldBe 0)
  }

  def assertLeft(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TFS.Enabled)(errorMessage: String): Unit =
    assertDiffEi(preconditions, block, fs)(_ should produce(errorMessage))

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  def zipWithPrev[A](seq: Seq[A]): Seq[(Option[A], A)] = {
    seq.zipWithIndex.map { case (a, i) => (if (i == 0) None else Some(seq(i - 1)), a) }
  }
}
