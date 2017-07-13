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
    val state = newState()
    val history = newHistory()
    val differ: (StateReader, History, Block) => Either[ValidationError, BlockDiff] = (s, h, b) => BlockDiffer.fromBlock(fs, s, h)(b)

    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, history, precondition).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }
    val totalDiff1 = differ(state, history, block)
    assertion(totalDiff1)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(fs, newState(), newHistory())(preconditions)
    val compositeState = new CompositeStateReader(newState(), preconditionDiff)
    val totalDiff2 = differ(compositeState, history, block)
    assertion(totalDiff2)
  }

  def assertDiffAndState(preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TestFunctionalitySettings.Enabled)(assertion: (BlockDiff, StateReader) => Unit): Unit = {
    val state = newState()
    val history = newHistory()
    val differ: (StateReader, History, Block) => Either[ValidationError, BlockDiff] = (s, h, b) => BlockDiffer.fromBlock(fs, s, h)(b)
    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, history, precondition).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }
    val totalDiff1 = differ(state, history, block).explicitGet()
    state.applyBlockDiff(totalDiff1)
    assertion(totalDiff1, state)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(fs, newState(), newHistory())(preconditions)
    val compositeState = new CompositeStateReader(newState(), preconditionDiff)
    val totalDiff2 = differ(compositeState, newHistory(), block).explicitGet()
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
