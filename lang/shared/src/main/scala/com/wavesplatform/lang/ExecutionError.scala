package com.wavesplatform.lang

sealed trait ExecutionError {
  def message: String
}
case class CommonError(message: String)                                             extends ExecutionError
case class FailOrRejectError(message: String, skipInvokeComplexity: Boolean = true) extends ExecutionError with ValidationError
