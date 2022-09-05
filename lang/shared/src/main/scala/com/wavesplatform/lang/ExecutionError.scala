package com.wavesplatform.lang

sealed trait ExecutionError {
  def message: String
}
case class CommonError(message: String, cause: Option[ValidationError] = None) extends ExecutionError {
  override def toString: String = s"CommonError($message)"
}
case class FailOrRejectError(message: String, skipInvokeComplexity: Boolean = true) extends ExecutionError with ValidationError
