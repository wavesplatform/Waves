package com.wavesplatform.features

case class InvalidFunctionalityError(message: String)

object InvalidFunctionalityError {
  def allowUntilTimestamp(ts: Long): InvalidFunctionalityError = InvalidFunctionalityError(s"not available after time=$ts")
  def allowAfterTimestamp(ts: Long): InvalidFunctionalityError = InvalidFunctionalityError(s"not available before time=$ts")
  def requiredAfterTimestamp(ts: Long) : InvalidFunctionalityError = InvalidFunctionalityError(s"required after time=$ts")
}
