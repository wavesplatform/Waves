package com.wavesplatform.mining

import scorex.block.Block
import scorex.transaction.Transaction

trait MiningSpace {
  def isEmpty: Boolean
  def isOverfilled: Boolean
  def empty: MiningSpace
  def put(x: Block): MiningSpace
  def put(x: Transaction): MiningSpace
}

case class OneDimensionMiningSpace(restSpace: Long, estimator: SpaceEstimator) extends MiningSpace {
  override def isEmpty: Boolean = restSpace <= 0
  override def isOverfilled: Boolean = restSpace < 0
  override def empty: OneDimensionMiningSpace = copy(restSpace = 0)
  override def put(x: Block): OneDimensionMiningSpace = put(estimator.estimate(x))
  override def put(x: Transaction): OneDimensionMiningSpace = put(estimator.estimate(x))
  private def put(x: Long): OneDimensionMiningSpace = copy(restSpace = this.restSpace - x)
}

object OneDimensionMiningSpace {
  def full(estimator: SpaceEstimator): OneDimensionMiningSpace = new OneDimensionMiningSpace(estimator.max, estimator)
}

case class TwoDimensionMiningSpace(first: MiningSpace, second: MiningSpace) extends MiningSpace {
  override def isEmpty: Boolean = first.isEmpty || second.isEmpty
  override def isOverfilled: Boolean = first.isOverfilled || second.isOverfilled
  override def empty: TwoDimensionMiningSpace = copy(first.empty, second.empty)
  override def put(x: Block): TwoDimensionMiningSpace = TwoDimensionMiningSpace(first.put(x), second.put(x))
  override def put(x: Transaction): TwoDimensionMiningSpace = TwoDimensionMiningSpace(first.put(x), second.put(x))
}

object TwoDimensionMiningSpace {
  def full(first: SpaceEstimator, second: SpaceEstimator): TwoDimensionMiningSpace = new TwoDimensionMiningSpace(
    first = OneDimensionMiningSpace.full(first),
    second = OneDimensionMiningSpace.full(second)
  )
  def partial(firstRest: MiningSpace, secondRest: MiningSpace): TwoDimensionMiningSpace = new TwoDimensionMiningSpace(firstRest, secondRest)
}
