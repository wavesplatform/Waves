package com.wavesplatform.it.generator

import org.scalatest.{FunSpecLike, Matchers}

class DistributedRandomGeneratorTest extends FunSpecLike with Matchers {

  private val first  = "first"
  private val second = "second"
  private val third  = "third"

  private val sampleCount = 100000
  private val lambda      = 0.01
  private def checkProbabilities(frequencies: Map[String, Double]): Boolean = {
    val generator = new DistributedRandomGenerator(frequencies)
    val resultMap = new scala.collection.mutable.HashMap[String, Int]()

    Range(0, sampleCount).foreach { _ =>
      val sample = generator.getRandom
      val count  = resultMap.getOrElse(sample, 0) + 1
      resultMap.put(sample, count)
    }

    resultMap.forall {
      case (element, count) =>
        val expectedProbability = generator.elementProbability(element)
        val realProbability     = count / sampleCount.toDouble
        Math.abs(expectedProbability - realProbability) <= lambda
    }
  }

  it("throw exception if frequencies is empty") {
    the[AssertionError] thrownBy new DistributedRandomGenerator(Map.empty[String, Double]) should have message "assertion failed: frequencies should not be empty"
  }

  it("work if frequencies map have one element") {
    assert(checkProbabilities(Map(first -> 0.1)))
    assert(checkProbabilities(Map(first -> 15)))
  }

  it("work if frequencies sum > 1") {
    val frequencies = Map(first -> 0.5, second -> 0.5, third -> 0.5)
    assert(checkProbabilities(frequencies))
  }

  it("work if frequencies sum < 1") {
    val frequencies = Map(first -> 0.1, second -> 0.2, third -> 0.01)
    assert(checkProbabilities(frequencies))
  }

  it("work if frequencies sum == 1") {
    val frequencies = Map(first -> 0.3, second -> 0.5, third -> 0.2)
    assert(checkProbabilities(frequencies))
  }

  it("work with a lot elements") {
    val frequencies = Map(
      "1" -> 0.1,
      "2" -> 0.01,
      "3" -> 0.07,
      "4" -> 0.009,
      "5" -> 0.15,
      "6" -> 0.05,
      "7" -> 0.08,
      "8" -> 0.77
    )
    assert(checkProbabilities(frequencies))
  }
}
