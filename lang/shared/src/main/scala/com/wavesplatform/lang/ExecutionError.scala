package com.wavesplatform.lang

sealed trait ExecutionError {
  def message: String
}
case class CommonError(message: String) extends ExecutionError
case class AlwaysRejectError(message: String) extends ExecutionError with ValidationError
