package com.wavesplatform.state

import cats.Monoid
import com.wavesplatform.block.Block
import com.wavesplatform.common.state.diffs.ProduceError
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.db.WithState
import com.wavesplatform.lagonaki.mocks.TestBlock
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.mining.MiningConstraint
import com.wavesplatform.settings.{FunctionalitySettings, TestFunctionalitySettings => TFS}
import com.wavesplatform.state.reader.CompositeBlockchain
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import org.scalatest.Matchers

package object diffs extends WithState with Matchers {
  val ENOUGH_AMT: Long = Long.MaxValue / 3

  def assertDiffEi(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TFS.Enabled)(
      assertion: Either[ValidationError, Diff] => Unit): Unit = withLevelDBWriter(fs) { state =>
    def differ(blockchain: Blockchain, b: Block) = BlockDiffer.fromBlock(blockchain, None, b, MiningConstraint.Unlimited)

    preconditions.foreach { precondition =>
      val BlockDiffer.Result(preconditionDiff, preconditionFees, totalFee, _) = differ(state, precondition).explicitGet()
      state.append(preconditionDiff, preconditionFees, totalFee, precondition)
    }
    val totalDiff1 = differ(state, block)
    assertion(totalDiff1.map(_.diff))
  }

  def assertDiffEiTraced(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TFS.Enabled)(
      assertion: TracedResult[ValidationError, Diff] => Unit): Unit = withLevelDBWriter(fs) { state =>
    def differ(blockchain: Blockchain, b: Block) = BlockDiffer.fromBlockTraced(blockchain, None, b, MiningConstraint.Unlimited)

    preconditions.foreach { precondition =>
      val BlockDiffer.Result(preconditionDiff, preconditionFees, totalFee, _) = differ(state, precondition).resultE.explicitGet()
      state.append(preconditionDiff, preconditionFees, totalFee, precondition)
    }
    val totalDiff1 = differ(state, block)
    assertion(totalDiff1.map(_.diff))
  }

  private def assertDiffAndState(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings, withNg: Boolean)(
      assertion: (Diff, Blockchain) => Unit): Unit = withLevelDBWriter(fs) { state =>
    def differ(blockchain: Blockchain, prevBlock: Option[Block], b: Block): Either[ValidationError, BlockDiffer.GenResult] =
      BlockDiffer.fromBlock(blockchain, if (withNg) prevBlock else None, b, MiningConstraint.Unlimited)

    preconditions.foldLeft[Option[Block]](None) { (prevBlock, curBlock) =>
      val BlockDiffer.Result(diff, fees, totalFee, _) = differ(state, prevBlock, curBlock).explicitGet()
      state.append(diff, fees, totalFee, curBlock)
      Some(curBlock)
    }

    val BlockDiffer.Result(diff, fees, totalFee, _) = differ(state, preconditions.lastOption, block).explicitGet()
    val cb              = CompositeBlockchain(state, Some(diff))
    assertion(diff, cb)

    state.append(diff, fees, totalFee, block)
    assertion(diff, state)
  }

  def assertNgDiffState(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TFS.Enabled)(
      assertion: (Diff, Blockchain) => Unit): Unit =
    assertDiffAndState(preconditions, block, fs, withNg = true)(assertion)

  def assertDiffAndState(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TFS.Enabled)(
      assertion: (Diff, Blockchain) => Unit): Unit =
    assertDiffAndState(preconditions, block, fs, withNg = false)(assertion)

  def assertDiffAndState(fs: FunctionalitySettings)(test: (Seq[Transaction] => Either[ValidationError, Unit]) => Unit): Unit =
    withLevelDBWriter(fs) { state =>
      def differ(blockchain: Blockchain, b: Block) = BlockDiffer.fromBlock(blockchain, None, b, MiningConstraint.Unlimited)

      test(txs => {
        val block = TestBlock.create(txs)
        differ(state, block).map(diff => state.append(diff.diff, diff.carry, diff.totalFee, block))
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
}
