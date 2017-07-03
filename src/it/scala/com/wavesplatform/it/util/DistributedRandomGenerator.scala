package com.wavesplatform.it.util

import scala.collection.immutable.TreeMap

class DistributedRandomGenerator[T](probabilities: Map[T, Float]) {
  assert(probabilities.values.sum == 1)
  def getRandom: T = {
    val rand = Math.random
    val g = probabilities.foldLeft((TreeMap[Float, Option[T]](0f -> None), 0f)) { case ((m, max), (t, prob)) =>
      val limit = max + prob
      (m.updated(limit, Some(t)), limit)
    }._1
    g.toSeq.sliding(2).find {
      case Seq((from, _), (to, _)) =>
        rand >= from && rand <= to
    }.flatMap(_.last._2).get
  }
}
