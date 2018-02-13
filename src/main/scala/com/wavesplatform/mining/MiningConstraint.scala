package com.wavesplatform.mining

import scorex.block.Block
import scorex.transaction.Transaction

trait MiningConstraint {
  def isEmpty: Boolean
  def isOverfilled: Boolean
  def put(x: Block): MiningConstraint
  def put(x: Transaction): MiningConstraint
}

case class OneDimensionalMiningConstraint(rest: Long, estimator: Estimator) extends MiningConstraint {
  override def isEmpty: Boolean = rest <= 0
  override def isOverfilled: Boolean = rest < 0
  override def put(x: Block): OneDimensionalMiningConstraint = put(estimator.estimate(x))
  override def put(x: Transaction): OneDimensionalMiningConstraint = put(estimator.estimate(x))
  private def put(x: Long): OneDimensionalMiningConstraint = copy(rest = this.rest - x)
}

object OneDimensionalMiningConstraint {
  def full(estimator: Estimator): OneDimensionalMiningConstraint = new OneDimensionalMiningConstraint(estimator.max, estimator)
}

case class TwoDimensionalMiningConstraint(first: MiningConstraint, second: MiningConstraint) extends MiningConstraint {
  override def isEmpty: Boolean = first.isEmpty || second.isEmpty
  override def isOverfilled: Boolean = first.isOverfilled || second.isOverfilled
  override def put(x: Block): TwoDimensionalMiningConstraint = TwoDimensionalMiningConstraint(first.put(x), second.put(x))
  override def put(x: Transaction): TwoDimensionalMiningConstraint = TwoDimensionalMiningConstraint(first.put(x), second.put(x))
}

object TwoDimensionalMiningConstraint {
  def full(first: Estimator, second: Estimator): TwoDimensionalMiningConstraint = new TwoDimensionalMiningConstraint(
    first = OneDimensionalMiningConstraint.full(first),
    second = OneDimensionalMiningConstraint.full(second)
  )
  def partial(firstRest: MiningConstraint, secondRest: MiningConstraint): TwoDimensionalMiningConstraint = new TwoDimensionalMiningConstraint(firstRest, secondRest)
}
