package com.wavesplatform.mining

import cats.data.NonEmptyList
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.Transaction

trait MiningConstraint {
  def isEmpty: Boolean
  def isOverfilled: Boolean
  def put(blockchain: Blockchain, x: Transaction): MiningConstraint
}

object MiningConstraint {
  case object Unlimited extends MiningConstraint {
    override def isEmpty: Boolean                                              = false
    override def isOverfilled: Boolean                                         = false
    override def put(blockchain: Blockchain, x: Transaction): MiningConstraint = this
  }
}

case class OneDimensionalMiningConstraint(rest: Long, estimator: TxEstimators.Fn, description: String = "") extends MiningConstraint {
  override def isEmpty: Boolean = {
    rest < estimator.minEstimate
  }
  override def isOverfilled: Boolean = {
    rest < 0
  }
  override def put(blockchain: Blockchain, x: Transaction): OneDimensionalMiningConstraint = put(estimator(blockchain, x))
  private def put(x: Long): OneDimensionalMiningConstraint = {
    copy(rest = this.rest - x)
  }

  override def toString: String = {
    s"MiningConstraint(${if (description.isEmpty) "???" else description}, rest = $rest, estimator = $estimator)"
  }
}

case class MultiDimensionalMiningConstraint(constraints: NonEmptyList[MiningConstraint]) extends MiningConstraint {
  override def isEmpty: Boolean      = constraints.exists(_.isEmpty)
  override def isOverfilled: Boolean = constraints.exists(_.isOverfilled)
  override def put(blockchain: Blockchain, x: Transaction): MultiDimensionalMiningConstraint =
    MultiDimensionalMiningConstraint(constraints.map(_.put(blockchain, x)))
}

object MultiDimensionalMiningConstraint {
  def apply(constraint1: MiningConstraint, constraint2: MiningConstraint): MultiDimensionalMiningConstraint =
    MultiDimensionalMiningConstraint(NonEmptyList.of(constraint1, constraint2))

  def formatOverfilledConstraints(currRest: MultiDimensionalMiningConstraint, updatedRest: MultiDimensionalMiningConstraint): Iterator[String] = {
    if (currRest.constraints.length == updatedRest.constraints.length) {
      (for ((curr, upd) <- currRest.constraints.toList.iterator.zip(updatedRest.constraints.toList.iterator) if upd.isOverfilled)
        yield
          (curr, upd) match {
            case (OneDimensionalMiningConstraint(rest, description, _), OneDimensionalMiningConstraint(newRest, _, _)) =>
              Iterator.single(s"$description($rest -> $newRest)")

            case (m: MultiDimensionalMiningConstraint, m1: MultiDimensionalMiningConstraint) =>
              Iterator.empty ++ formatOverfilledConstraints(m, m1)

            case _ =>
              Iterator.single(s"$curr -> $upd")
          }).flatten
    } else {
      updatedRest.constraints.toList.iterator
        .filter(_.isOverfilled)
        .map(_.toString)
    }
  }
}
