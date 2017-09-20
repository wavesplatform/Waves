package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.Functionalities
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import scorex.block.Block
import scorex.settings.TestFunctionality
import scorex.transaction.{History, ValidationError}

package object diffs {

  private val testFunctionalities = new Functionalities(TestFunctionality.EnabledSettings, TestFunctionality.EnabledProvider)
  private val lock = new ReentrantReadWriteLock()

  def newState(): StateWriterImpl = new StateWriterImpl(StateStorage(None, dropExisting = false).get, lock)

  def newHistory(): History = HistoryWriterImpl(None, lock, TestFunctionality.EnabledSettings).get

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  def assertDiffEi(preconditions: Seq[Block], block: Block, fn: Functionalities = testFunctionalities)
                  (assertion: Either[ValidationError, BlockDiff] => Unit): Unit = {
    val differ: (StateReader, Block) => Either[ValidationError, BlockDiff] = (s, b) => BlockDiffer.fromBlock(fn, s, None)(b)

    val state = newState()

    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, precondition).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }
    val totalDiff1 = differ(state, block)
    assertion(totalDiff1)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(fn, newState(), None)(preconditions)
    val compositeState = new CompositeStateReader(newState(), preconditionDiff)
    val totalDiff2 = differ(compositeState, block)
    assertion(totalDiff2)
  }

  def assertDiffAndState(preconditions: Seq[Block], block: Block, fn: Functionalities = testFunctionalities)
                        (assertion: (BlockDiff, StateReader) => Unit): Unit = {
    val differ: (StateReader, Block) => Either[ValidationError, BlockDiff] = (s, b) => BlockDiffer.fromBlock(fn, s, None)(b)

    val state = newState()
    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, precondition).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }
    val totalDiff1 = differ(state, block).explicitGet()
    state.applyBlockDiff(totalDiff1)
    assertion(totalDiff1, state)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(fn, newState(), None)(preconditions)
    val compositeState = new CompositeStateReader(newState(), preconditionDiff)
    val totalDiff2 = differ(compositeState, block).explicitGet()
    assertion(totalDiff2, new CompositeStateReader(compositeState, totalDiff2))
  }

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)
}
