package com.wavesplatform.generator

import scala.collection.immutable.TreeMap
import scala.util.Random

// https://en.wikipedia.org/wiki/Categorical_distribution
trait DistributedRandomGenerator[T] {
  def getRandom: T
}

object DistributedRandomGenerator {
  private class SingleOutcome[T](outcome: T) extends DistributedRandomGenerator[T] {
    override def getRandom: T = outcome
  }

  private class MultipleOutcomes[T](outcomes: TreeMap[Double, T], total: Double) extends DistributedRandomGenerator[T] {
    override def getRandom: T = outcomes.from(Random.nextDouble() * total).head._2
  }

  def apply[T](probabilities: Map[T, Double]): DistributedRandomGenerator[T] = {
    val filteredProbabilities = probabilities.filter { case (_, p) => p > 0 }
    filteredProbabilities.size match {
      case 0 => throw new IllegalArgumentException("empty probability list")
      case 1 => new SingleOutcome[T](filteredProbabilities.head._1)
      case _ =>
        val (treeMap, total) = filteredProbabilities.foldLeft((TreeMap.empty[Double, T], 0d)) {
          case ((tm, acc), (v, p)) => (tm + ((acc + p) -> v), acc + p)
        }

        new MultipleOutcomes[T](treeMap, total)
    }
  }
}
