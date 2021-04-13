package com.wavesplatform.lang

sealed trait ExecutionError
object ExecutionError {
  final case class Untyped(message: String) extends ExecutionError {
    override def toString: String = message
  }
  final case class Typed(error: ValidationError) extends ExecutionError {
    override def toString: String = error.toString
  }

  //noinspection LanguageFeature
  // FIXME make this explicit
  implicit def apply(message: String): ExecutionError = Untyped(message)
  def apply(error: ValidationError): ExecutionError   = Typed(error)
}
