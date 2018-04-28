package com.wavesplatform.mining

import cats.data.NonEmptyList
import scorex.block.Block
import scorex.transaction.Transaction

trait MiningConstraint {
  def isEmpty: Boolean
  def isOverfilled: Boolean
  def put(x: Block): MiningConstraint
  def put(x: Transaction): MiningConstraint
}

case class OneDimensionalMiningConstraint(rest: Long, estimator: Estimator) extends MiningConstraint {
  override def isEmpty: Boolean                                    = rest <= 0
  override def isOverfilled: Boolean                               = rest < 0
  override def put(x: Block): OneDimensionalMiningConstraint       = put(estimator.estimate(x))
  override def put(x: Transaction): OneDimensionalMiningConstraint = put(estimator.estimate(x))
  private def put(x: Long): OneDimensionalMiningConstraint         = copy(rest = this.rest - x)
}

object OneDimensionalMiningConstraint {
  def full(estimator: Estimator): OneDimensionalMiningConstraint = new OneDimensionalMiningConstraint(estimator.max, estimator)
}

case class MultiDimensionalMiningConstraint(constraints: NonEmptyList[MiningConstraint]) extends MiningConstraint {
  override def isEmpty: Boolean                                      = constraints.exists(_.isEmpty)
  override def isOverfilled: Boolean                                 = constraints.exists(_.isOverfilled)
  override def put(x: Block): MultiDimensionalMiningConstraint       = MultiDimensionalMiningConstraint(constraints.map(_.put(x)))
  override def put(x: Transaction): MultiDimensionalMiningConstraint = MultiDimensionalMiningConstraint(constraints.map(_.put(x)))
}

object MultiDimensionalMiningConstraint {
  def full(estimators: NonEmptyList[Estimator]): MultiDimensionalMiningConstraint =
    MultiDimensionalMiningConstraint(estimators.map(OneDimensionalMiningConstraint.full))

  def full(estimator1: Estimator, estimator2: Estimator): MultiDimensionalMiningConstraint =
    MultiDimensionalMiningConstraint(NonEmptyList.of(estimator1, estimator2).map(OneDimensionalMiningConstraint.full))

  def partial(constraint1: MiningConstraint, constraint2: MiningConstraint): MultiDimensionalMiningConstraint =
    MultiDimensionalMiningConstraint(NonEmptyList.of(constraint1, constraint2))
}
