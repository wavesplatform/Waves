package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.features.FeatureProvider
import com.wavesplatform.history.HistoryWriterImpl
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.CompositeStateReader.composite
import com.wavesplatform.state2.reader.SnapshotStateReader
import org.iq80.leveldb.DB
import org.scalatest.Matchers
import scorex.block.Block
import scorex.settings.TestFunctionalitySettings
import scorex.transaction.{History, ValidationError}

package object diffs extends Matchers {

  private val lock = new ReentrantReadWriteLock()

  def newState(db: DB, storeTransactions: Boolean = true): StateWriterImpl = {
    val s = new StateWriterImpl(StateStorage(db, dropExisting = false).get, new ReentrantReadWriteLock())
    s.clear()
    s
  }

  def newHistory(db: DB, fs: FunctionalitySettings = TestFunctionalitySettings.Enabled): History with FeatureProvider =
    HistoryWriterImpl(db, lock, fs, TestFunctionalitySettings.EmptyFeaturesSettings).get

  val ENOUGH_AMT: Long = Long.MaxValue / 3

  def assertDiffEi(db: DB, preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TestFunctionalitySettings.Enabled)(assertion: Either[ValidationError, BlockDiff] => Unit): Unit = {
    val fp = newHistory(db, fs)
    val state = newState(db)
    val differ: (SnapshotStateReader, Block) => Either[ValidationError, BlockDiff] = (s, b) => BlockDiffer.fromBlock(fs, fp, s, None, b)

    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, precondition).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }
    val totalDiff1 = differ(state, block)
    assertion(totalDiff1)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(fs, fp, newState(db), None, 6)(preconditions)
    val compositeState = composite(preconditionDiff, newState(db))
    val totalDiff2 = differ(compositeState, block)
    assertion(totalDiff2)
  }

  def assertLeft(db: DB, preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TestFunctionalitySettings.Enabled)(errorMessage: String): Unit
  = assertDiffEi(db, preconditions, block, fs)(_ should produce(errorMessage))

  def assertDiffAndState(db: DB, preconditions: Seq[Block], block: Block, fs: FunctionalitySettings = TestFunctionalitySettings.Enabled)(assertion: (BlockDiff, SnapshotStateReader) => Unit): Unit = {
    val fp = newHistory(db, fs)
    val state = newState(db)

    val differ: (SnapshotStateReader, Block) => Either[ValidationError, BlockDiff] = (s, b) => BlockDiffer.fromBlock(fs, fp, s, None, b)

    preconditions.foreach { precondition =>
      val preconditionDiff = differ(state, precondition).explicitGet()
      state.applyBlockDiff(preconditionDiff)
    }
    val totalDiff1 = differ(state, block).explicitGet()
    state.applyBlockDiff(totalDiff1)
    assertion(totalDiff1, state)

    val preconditionDiff = BlockDiffer.unsafeDiffMany(fs, fp, newState(db), None, 7)(preconditions)
    val compositeState = composite(preconditionDiff, newState(db))
    val totalDiff2 = differ(compositeState, block).explicitGet()
    assertion(totalDiff2, composite(totalDiff2, compositeState))
  }

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  def zipWithPrev[A](seq: Seq[A]): Seq[(Option[A], A)] = {
    seq.zipWithIndex.map { case ((a, i)) => (if (i == 0) None else Some(seq(i - 1)), a) }
  }
}
