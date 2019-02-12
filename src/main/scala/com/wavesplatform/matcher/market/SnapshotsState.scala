package com.wavesplatform.matcher.market

import com.wavesplatform.matcher.queue.QueueEventWithMeta.{Offset => EventOffset}
import com.wavesplatform.transaction.assets.exchange.AssetPair
import com.wavesplatform.utils.doWhile

import scala.collection.immutable.SortedSet

/**
  * nearestSnapshotOffsets should be a PriorityQueue
  */
case class SnapshotsState private (startOffsetToSnapshot: EventOffset,
                                   snapshotOffsets: Map[AssetPair, EventOffset],
                                   nearestSnapshotOffsets: SortedSet[(AssetPair, EventOffset)]) {
  import SnapshotsState._

  val nearestSnapshotOffset: Option[(AssetPair, EventOffset)] = nearestSnapshotOffsets.headOption // Caching log(N) operations

  def requiredSnapshot(offset: EventOffset): Option[(AssetPair, SnapshotsState)] =
    if (offset > startOffsetToSnapshot) nearestSnapshotOffset.collect {
      case (assetPair, nearestSnapshotNr) if offset >= nearestSnapshotNr =>
        assetPair -> copy(nearestSnapshotOffsets = this.nearestSnapshotOffsets.tail)
    } else None

  def updated(assetPair: AssetPair, snapshotEventNr: EventOffset, lastGlobalEventNr: EventOffset, interval: EventOffset): SnapshotsState = {
    val nextSnapshotOffset = {
      val z = (snapshotEventNr / interval) * interval + snapshotOffset(assetPair, interval)
      doWhile(z)(_ <= lastGlobalEventNr)(_ + interval)
    }

    copy(
      snapshotOffsets = snapshotOffsets.updated(assetPair, snapshotEventNr),
      nearestSnapshotOffsets = nearestSnapshotOffsets + (assetPair -> nextSnapshotOffset)
    )
  }
}

object SnapshotsState {
  val empty = SnapshotsState(
    startOffsetToSnapshot = Long.MinValue,
    snapshotOffsets = Map.empty,
    nearestSnapshotOffsets = SortedSet.empty(Ordering.by[(AssetPair, EventOffset), (EventOffset, String)] {
      case (assetPair, offset) => (offset, assetPair.key)
    })
  )

  def apply(startOffsetToSnapshot: Long,
            currentOffsets: Map[AssetPair, EventOffset],
            lastProcessedNr: EventOffset,
            interval: EventOffset): SnapshotsState = empty.copy(
    startOffsetToSnapshot = startOffsetToSnapshot,
    snapshotOffsets = currentOffsets,
    nearestSnapshotOffsets = empty.nearestSnapshotOffsets ++ currentOffsets.map { // ++ to preserve ordering
      case (assetPair, o) =>
        val nextSnapshotOffset = {
          val z = (o / interval) * interval + snapshotOffset(assetPair, interval)
          doWhile(z)(_ <= lastProcessedNr)(_ + interval)
        }

        assetPair -> (o + nextSnapshotOffset)
    }
  )

  private def snapshotOffset(assetPair: AssetPair, interval: EventOffset): Long = math.abs(assetPair.key.hashCode % interval)
}
