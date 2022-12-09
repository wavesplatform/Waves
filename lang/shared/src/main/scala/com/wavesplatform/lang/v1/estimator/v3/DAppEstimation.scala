package com.wavesplatform.lang.v1.estimator.v3

case class DAppEstimation(
    maxComplexity: (String, Long),
    annotatedComplexities: Map[String, Long],
    globalLetsCosts: Map[String, Long],
    globalFunctionsCosts: Map[String, Long]
)
