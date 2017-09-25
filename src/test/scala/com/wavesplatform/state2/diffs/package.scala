package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.block.Block
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.{History, ValidationError}

package object diffs {

  private val lock = new ReentrantReadWriteLock()

  def newState(): StateWriterImpl = new StateWriterImpl(StateStorage(None, dropExisting = false).get, lock)

  def newHistory(): History = HistoryWriterImpl(None, lock, TestFunctionalitySettings.Enabled, TestFunctionalitySettings.EmptyFeaturesSettings).get

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  def assertDiffEi(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TestFunctionalitySettings.Enabled)(assertion: Either[ValidationError, BlockDiff] => Unit): Unit = {
    val differ: (StateReader, Block) => Either[ValidationError, BlockDiff] = (s, b) => BlockDiffer.fromBlock(fs, s, None, b)

    val state = newState()

    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, precondition).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }
    val totalDiff1 = differ(state, block)
    assertion(totalDiff1)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(fs, newState(), None)(preconditions)
    val compositeState = new CompositeStateReader(newState(), preconditionDiff)
    val totalDiff2 = differ(compositeState, block)
    assertion(totalDiff2)
  }

  def assertDiffEiWithPrev(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TestFunctionalitySettings.Enabled)(assertion: (BlockDiff, StateReader) => Unit): Unit = {
    val differ: (StateReader, (Option[Block], Block)) => Either[ValidationError, BlockDiff] = {
      case (s, (prev, b)) => BlockDiffer.fromBlock(fs, s, prev, b)
    }

    val state = newState()

    zipWithPrev(preconditions).foreach { wp =>
      val preconditionDiff = differ(state, wp).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }

    val totalDiff1 = differ(state, (preconditions.lastOption, block)).explicitGet()
    state.applyBlockDiff(totalDiff1)
    assertion(totalDiff1, state)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(fs, newState(), None)(preconditions)
    val compositeState = new CompositeStateReader(newState(), preconditionDiff)
    val totalDiff2 = differ(compositeState, (preconditions.lastOption, block)).explicitGet()
    assertion(totalDiff2, CompositeStateReader.composite(compositeState, () => totalDiff2))

    assert(totalDiff1 == totalDiff2)
  }

  def assertDiffAndState(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TestFunctionalitySettings.Enabled)(assertion: (BlockDiff, StateReader) => Unit): Unit = {
    val differ: (StateReader, Block) => Either[ValidationError, BlockDiff] = (s, b) => BlockDiffer.fromBlock(fs, s, None, b)

    val state = newState()
    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, precondition).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }
    val totalDiff1 = differ(state, block).explicitGet()
    state.applyBlockDiff(totalDiff1)
    assertion(totalDiff1, state)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(fs, newState(), None)(preconditions)
    val compositeState = new CompositeStateReader(newState(), preconditionDiff)
    val totalDiff2 = differ(compositeState, block).explicitGet()
    assertion(totalDiff2, new CompositeStateReader(compositeState, totalDiff2))
  }

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  def zipWithPrev[A](seq: Seq[A]): Seq[(Option[A], A)] = {
    seq.zipWithIndex.map { case ((a, i)) => (if (i == 0) None else Some(seq(i - 1)), a) }
  }
}
