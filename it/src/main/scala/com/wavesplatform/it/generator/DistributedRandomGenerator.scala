package com.wavesplatform.it.generator

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

class DistributedRandomGenerator[T](frequencies: Map[T, Double]) {
  assert(frequencies.nonEmpty, "frequencies should not be empty")

  private val frequenciesSum: Double = frequencies.values.sum
  private val distribution = {
    var counter = 0.0
    val result = frequencies.toList.map {
      case (generator, frequency) =>
        val probability = frequency / frequenciesSum
        counter = counter + probability
        counter -> generator
    }
    TreeMap[Double, T](result: _*)
  }

  @tailrec
  final def getRandom: T = {
    val rand = Math.random
    val optionResult = distribution.find {
      case (prob, _) => rand <= prob
    }

    optionResult match {
      case None                 => getRandom
      case Some((_, generator)) => generator
    }
  }

  def elementProbability(element: T): Double = {
    frequencies.getOrElse(element, 0.0) / frequenciesSum
  }
}
