package com.wavesplatform.lang.v1.estimator.v3
import cats.implicits.toFoldableOps

case class DAppEstimation(
    annotatedComplexities: Map[String, Long],
    globalLetsCosts: Map[String, Long],
    globalFunctionsCosts: Map[String, Long]
) {
  val maxAnnotatedComplexity: (String, Long) =
    annotatedComplexities.toList.maximumOption(_._2 compareTo _._2).getOrElse(("", 0L))
}
