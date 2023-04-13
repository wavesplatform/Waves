package com.wavesplatform.ride.runner.db

import com.wavesplatform.state.Height

object Heights {
  val MaxRollbackSize  = 100
  val SafeHeightOffset = MaxRollbackSize + 1

  /** @return
    *   (toPreserve, toRemove)
    */
  def splitHeightsAt(removeFrom: Height, heights: Heights): (Heights, Heights) =
    if (heights.isEmpty) (heights, heights)
    else {
      val firstIdxToPreserve = heights.indexWhere(_ < removeFrom)
      if (firstIdxToPreserve == -1) (Vector.empty, heights)
      else heights.splitAt(firstIdxToPreserve).swap
    }

  /** @return
    *   (safeToPreserve, safeToRemove)
    */
  def splitHeightsAtRollback(curr: Height, heights: Heights): (Heights, Heights) =
    if (heights.isEmpty) (heights, heights)
    else {
      val minSafeHeight         = curr - SafeHeightOffset
      val secondIndexToPreserve = heights.lastIndexWhere(_ > minSafeHeight)
      if (secondIndexToPreserve == -1) (Vector.empty, heights.take(1))
      else heights.splitAt(secondIndexToPreserve + 1)
    }
}
