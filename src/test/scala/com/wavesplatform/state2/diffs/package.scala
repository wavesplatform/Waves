package com.wavesplatform.state2

import com.wavesplatform.state2.reader.{CompositeStateReader, StateReader}
import org.h2.mvstore.MVStore
import org.scalatest.matchers.{MatchResult, Matcher}
import scorex.block.Block
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.ValidationError

import scala.util.{Left, Right}

package object diffs {


  def newState(): StateWriterImpl = new StateWriterImpl(new MVStorePrimitiveImpl(new MVStore.Builder().open()))

  val differ: (StateReader, Block) => Either[ValidationError, BlockDiff] = BlockDiffer(TestFunctionalitySettings.Enabled)
  val ENOUGH_AMT: Long = Long.MaxValue / 3

  def assertDiffEi(preconditions: Seq[Block], block: Block)(assertion: Either[ValidationError, BlockDiff] => Unit): Unit = {
    val state = newState()
    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, precondition).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }
    val totalDiff1 = differ(state, block)
    assertion(totalDiff1)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(TestFunctionalitySettings.Enabled)(newState(), preconditions)
    val compositeState = new CompositeStateReader(newState(), preconditionDiff)
    val totalDiff2 = differ(compositeState, block)
    assertion(totalDiff2)
  }

  def assertDiffAndState(preconditions: Seq[Block], block: Block)(assertion: (BlockDiff, StateReader) => Unit): Unit = {
    val state = newState()
    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, precondition).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }
    val totalDiff1 = differ(state, block).explicitGet()
    state.applyBlockDiff(totalDiff1)
    assertion(totalDiff1, state)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(TestFunctionalitySettings.Enabled)(newState(), preconditions)
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
