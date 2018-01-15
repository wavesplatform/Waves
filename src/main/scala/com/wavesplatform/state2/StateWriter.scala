package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import cats.implicits._
import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.state2.reader.StateReaderImpl
import scorex.utils.ScorexLogging

trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff): Unit

  def clear(): Unit
}

class StateWriterImpl(p: StateStorage, synchronizationToken: ReentrantReadWriteLock)
  extends StateReaderImpl(p, synchronizationToken) with StateWriter with ScorexLogging with Instrumented {

  override def applyBlockDiff(blockDiff: BlockDiff): Unit = write { implicit l =>
    val oldHeight = sp().getHeight
    val newHeight = oldHeight + blockDiff.heightDiff
    log.debug(s"Starting persist from $oldHeight to $newHeight")

    val b = sp().createBatch()

    measureLog("transactions")(sp().putTransactions(b, blockDiff.txsDiff))

    measureSizeLog("snapshots")(blockDiff.snapshots)(
      _.foreach { case (acc, snapshotsByHeight) =>
        snapshotsByHeight.foreach { case (h, snapshot) =>
          sp().putBalanceSnapshots(b, acc, h, (snapshot.prevHeight, snapshot.balance, snapshot.effectiveBalance))
        }
        sp().putLastBalanceSnapshotHeight(b, acc, snapshotsByHeight.keys.max)
      })

    sp().setHeight(b, newHeight)

    sp().commit(b)

    log.info(s"BlockDiff commit complete. Persisted height = $newHeight")
  }

  override def clear(): Unit = write { implicit l =>
    val b = sp().createBatch()
    sp().removeEverything(b)
    sp().setHeight(b, 0)
    sp().commit(b)
  }
}
