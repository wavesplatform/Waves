package com.wavesplatform.lang.v1.estimator.v3

case class GlobalDeclarationsCosts(
    globalLetsCosts: Map[String, Long],
    globalFunctionsCosts: Map[String, Long]
)
