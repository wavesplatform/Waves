package com.wavesplatform.mining

import cats.data.NonEmptyList
import com.wavesplatform.state.Blockchain
import scorex.block.Block
import scorex.transaction.Transaction

trait MiningConstraint {
  def isEmpty: Boolean
  def isOverfilled: Boolean
  def put(blockchain: Blockchain, x: Block): MiningConstraint
  def put(blockchain: Blockchain, x: Transaction): MiningConstraint
}

case class OneDimensionalMiningConstraint(rest: Long, estimator: Estimator) extends MiningConstraint {
  override def isEmpty: Boolean                                                            = rest <= 0
  override def isOverfilled: Boolean                                                       = rest < 0
  override def put(blockchain: Blockchain, x: Block): OneDimensionalMiningConstraint       = put(estimator.estimate(blockchain, x))
  override def put(blockchain: Blockchain, x: Transaction): OneDimensionalMiningConstraint = put(estimator.estimate(blockchain, x))
  private def put(x: Long): OneDimensionalMiningConstraint                                 = copy(rest = this.rest - x)
}

case class MultiDimensionalMiningConstraint(constraints: NonEmptyList[MiningConstraint]) extends MiningConstraint {
  override def isEmpty: Boolean      = constraints.exists(_.isEmpty)
  override def isOverfilled: Boolean = constraints.exists(_.isOverfilled)
  override def put(blockchain: Blockchain, x: Block): MultiDimensionalMiningConstraint =
    MultiDimensionalMiningConstraint(constraints.map(_.put(blockchain, x)))
  override def put(blockchain: Blockchain, x: Transaction): MultiDimensionalMiningConstraint =
    MultiDimensionalMiningConstraint(constraints.map(_.put(blockchain, x)))
}

object MultiDimensionalMiningConstraint {
  def apply(constraint1: MiningConstraint, constraint2: MiningConstraint): MultiDimensionalMiningConstraint =
    MultiDimensionalMiningConstraint(NonEmptyList.of(constraint1, constraint2))
}
