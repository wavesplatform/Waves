package com.wavesplatform.dexgen

import scala.collection.immutable.TreeMap

class DistributedRandomGenerator[T](probabilities: Map[T, Double]) {
  assert(probabilities.values.sum == 1)
  def getRandom: T = {
    val rand = Math.random
    val g = probabilities
      .foldLeft((TreeMap[Double, Option[T]](0d -> None), 0d)) {
        case ((m, max), (t, prob)) =>
          val limit = max + prob
          (m.updated(limit, Some(t)), limit)
      }
      ._1
    g.toSeq
      .sliding(2)
      .find {
        case Seq((from, _), (to, _)) =>
          rand >= from && rand <= to
      }
      .flatMap(_.last._2)
      .get
  }
}
