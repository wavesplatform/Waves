package com.wavesplatform.lang

sealed trait ExecutionError {
  def message: String
}
case class CommonError(details: String, cause: Option[ValidationError] = None) extends ExecutionError {
  override def message: String  = cause.map(_.toString).getOrElse(details)
}
case class ThrownError(message: String) extends ExecutionError
case class FailOrRejectError(message: String, skipInvokeComplexity: Boolean = true) extends ExecutionError with ValidationError

case class EvaluationException(cause: Throwable) extends ExecutionError {
  override lazy val message: String = s"class ${cause.getClass} ${String.valueOf(cause.getMessage)}"
}

case object SoftLimitReached extends ExecutionError {
  override val message = "Soft limit reached"
}
