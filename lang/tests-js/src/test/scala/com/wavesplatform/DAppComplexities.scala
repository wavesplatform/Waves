package com.wavesplatform

case class DAppComplexities(
    complexity: Int,
    verifierComplexity: Int,
    callableComplexities: Map[String, Int],
    userFunctionComplexities: Map[String, Int],
    globalVariableComplexities: Map[String, Int]
)
