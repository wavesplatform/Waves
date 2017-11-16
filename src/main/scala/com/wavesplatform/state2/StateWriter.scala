package com.wavesplatform.state2

import java.util.concurrent.locks.ReentrantReadWriteLock

import com.wavesplatform.metrics.Instrumented
import com.wavesplatform.state2.reader.StateReaderImpl
import scorex.utils.ScorexLogging

trait StateWriter {
  def applyBlockDiff(blockDiff: BlockDiff): Unit

  def clear(): Unit
}

class StateWriterImpl(p: StateStorage, synchronizationToken: ReentrantReadWriteLock)
  extends StateReaderImpl(p, synchronizationToken) with StateWriter with AutoCloseable with ScorexLogging with Instrumented {

  override def close(): Unit = p.close()

  override def applyBlockDiff(blockDiff: BlockDiff): Unit = write { implicit l =>
    val oldHeight = p.getHeight
    val newHeight = oldHeight + blockDiff.heightDiff
    log.debug(s"Starting persist from $oldHeight to $newHeight")

    val b = p.createBatch()

    measureLog("transactions")(p.putTransactions(b, blockDiff.txsDiff))

    measureSizeLog("snapshots")(blockDiff.snapshots)(
      _.foreach { case (acc, snapshotsByHeight) =>
        snapshotsByHeight.foreach { case (h, snapshot) =>
          p.putBalanceSnapshots(b, acc, h, (snapshot.prevHeight, snapshot.balance, snapshot.effectiveBalance))
        }
        p.putLastBalanceSnapshotHeight(b, acc, snapshotsByHeight.keys.max)
      })

    p.setHeight(b, newHeight)

    p.commit(b)

    log.info(s"BlockDiff commit complete. Persisted height = $newHeight")
  }

  override def clear(): Unit = write { implicit l =>
    val b = p.createBatch()
    p.removeEverything(b)
    p.setHeight(b, 0)
    p.commit(b)
  }
}
