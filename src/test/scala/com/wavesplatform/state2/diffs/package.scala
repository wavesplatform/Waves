package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import org.scalatest.matchers.{MatchResult, Matcher}
import scorex.block.Block
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.{History, ValidationError}

import scala.util.{Left, Right}

package object diffs {

  private val lock = new ReentrantReadWriteLock()

  def newState(): StateWriterImpl = new StateWriterImpl(StateStorage(None, dropExisting = false).get, lock)

  def newHistory(): History = HistoryWriterImpl(None, lock).get

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

  class ProduceError(errorMessage: String) extends Matcher[Either[_, _]] {
    override def apply(ei: Either[_, _]): MatchResult = {
      ei match {
        case r@Right(_) => MatchResult(matches = false, "expecting Left(...{0}...) but got {1}", "got expected error", IndexedSeq(errorMessage, r))
        case l@Left(_) => MatchResult(matches = l.toString contains errorMessage,
          "expecting Left(...{0}...) but got {1}", "got expected error", IndexedSeq(errorMessage, l))
      }
    }
  }

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)
}
