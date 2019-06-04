package com.wavesplatform.matcher.market

import com.wavesplatform.matcher.queue.QueueEventWithMeta.{Offset => EventOffset}
import com.wavesplatform.transaction.assets.exchange.AssetPair

import scala.collection.immutable.SortedSet

/**
  * @param snapshotOffsets        Map of pair -> [Some(current snapshot offset) | None if it doesn't exist]
  * @param nearestSnapshotOffsets Nearest (pair, offset) to do snapshot
  * @note nearestSnapshotOffsets should be a PriorityQueue
  */
case class SnapshotsState private (snapshotOffsets: Map[AssetPair, Option[EventOffset]],
                                   nearestSnapshotOffsets: SortedSet[(AssetPair, EventOffset)]) {
  import SnapshotsState._

  val nearestSnapshotOffset: Option[(AssetPair, EventOffset)] = nearestSnapshotOffsets.headOption // Caching log(N) operations

  /**
    * @return Some(assetPairX, stateWithoutThisAssetPairX) - if it is time to do a snapshot for assetPairX
    *         None - if we shouldn't do a snapshot at this offset
    */
  def requiredSnapshot(offset: EventOffset): Option[(AssetPair, SnapshotsState)] =
    nearestSnapshotOffset.collect {
      case (assetPair, x) if offset >= x =>
        assetPair -> copy(nearestSnapshotOffsets = this.nearestSnapshotOffsets.tail)
    }

  def updated(assetPair: AssetPair,
              currSnapshotOffset: Option[EventOffset],
              lastProcessedOffset: EventOffset,
              interval: EventOffset): SnapshotsState = {
    val nextOffset         = nextSnapshotOffset(assetPair, currSnapshotOffset.getOrElse(-1L), lastProcessedOffset, interval)
    val newSnapshotOffsets = snapshotOffsets.updated(assetPair, currSnapshotOffset)
    copy(
      snapshotOffsets = newSnapshotOffsets,
      nearestSnapshotOffsets = nearestSnapshotOffsets + (assetPair -> nextOffset)
    )
  }

  def without(assetPair: AssetPair): SnapshotsState = copy(
    snapshotOffsets = snapshotOffsets - assetPair,
    nearestSnapshotOffsets = nearestSnapshotOffsets.filterNot(_._1 == assetPair)
  )
}

object SnapshotsState {
  val empty = SnapshotsState(
    snapshotOffsets = Map.empty,
    nearestSnapshotOffsets = SortedSet.empty(Ordering.by[(AssetPair, EventOffset), (EventOffset, String)] {
      case (assetPair, offset) => (offset, assetPair.key)
    })
  )

  def apply(currentOffsets: Map[AssetPair, Option[EventOffset]], lastProcessedOffset: EventOffset, interval: EventOffset): SnapshotsState =
    empty.copy(
      snapshotOffsets = currentOffsets,
      nearestSnapshotOffsets = empty.nearestSnapshotOffsets ++ currentOffsets.map { // ++ to preserve ordering
        case (assetPair, currSnapshotOffset) =>
          assetPair -> nextSnapshotOffset(assetPair, currSnapshotOffset.getOrElse(-1L), lastProcessedOffset, interval)
      }
    )

  def nextSnapshotOffset(assetPair: AssetPair,
                         currSnapshotOffset: EventOffset,
                         lastProcessedOffset: EventOffset,
                         interval: EventOffset): EventOffset = {
    val prevOffset              = math.max(currSnapshotOffset, lastProcessedOffset)
    val z                       = snapshotOffset(assetPair, interval)
    val currIntervalStartOffset = (prevOffset / interval) * interval
    val r                       = currIntervalStartOffset + z
    if (r <= prevOffset) r + interval else r
  }

  /**
    * @return An offset for assetPair's snapshot E [0; interval]
    */
  private def snapshotOffset(assetPair: AssetPair, interval: EventOffset): EventOffset = math.abs(assetPair.key.hashCode % interval)
}
