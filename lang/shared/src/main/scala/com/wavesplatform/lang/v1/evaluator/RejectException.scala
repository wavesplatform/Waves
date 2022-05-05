package com.wavesplatform.lang.v1.evaluator

case class RejectException(error: String) extends RuntimeException(error)
