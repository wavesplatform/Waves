package com.wavesplatform.mining

import scorex.block.Block
import scorex.transaction.Transaction

trait MiningSpace {
  def isEmpty: Boolean
  def put(x: Block): Option[MiningSpace]
  def put(x: Transaction): Option[MiningSpace]
}

case class OneDimensionMiningSpace(restSpace: Long, estimator: SpaceEstimator) extends MiningSpace {
  override def isEmpty: Boolean = restSpace <= 0
  override def put(x: Block): Option[OneDimensionMiningSpace] = spend(estimator.estimate(x))
  override def put(x: Transaction): Option[OneDimensionMiningSpace] = spend(estimator.estimate(x))
  private def spend(x: Long): Option[OneDimensionMiningSpace] = {
    val updatedRestSpace = restSpace - x
    if (updatedRestSpace < 0) None else Some(OneDimensionMiningSpace(updatedRestSpace, estimator))
  }
}

object OneDimensionMiningSpace {
  def full(estimator: SpaceEstimator): OneDimensionMiningSpace = new OneDimensionMiningSpace(estimator.max, estimator)
}

case class TwoDimensionMiningSpace(first: MiningSpace, second: MiningSpace) extends MiningSpace {
  override def isEmpty: Boolean = first.isEmpty || second.isEmpty

  override def put(x: Block): Option[TwoDimensionMiningSpace] = (first.put(x), second.put(x)) match {
    case (Some(f), Some(s)) => Some(TwoDimensionMiningSpace(f, s))
    case _ => None
  }

  override def put(x: Transaction): Option[TwoDimensionMiningSpace] = (first.put(x), second.put(x)) match {
    case (Some(f), Some(s)) => Some(TwoDimensionMiningSpace(f, s))
    case _ => None
  }
}

object TwoDimensionMiningSpace {
  def full(first: SpaceEstimator, second: SpaceEstimator): TwoDimensionMiningSpace = new TwoDimensionMiningSpace(
    first = OneDimensionMiningSpace.full(first),
    second = OneDimensionMiningSpace.full(second)
  )
  def partial(firstRest: MiningSpace, secondRest: MiningSpace): TwoDimensionMiningSpace = new TwoDimensionMiningSpace(firstRest, secondRest)
}
