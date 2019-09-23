package com.wavesplatform.lang

import scala.util.Either

trait ValidationError extends Product with Serializable

object ValidationError {
  type Validation[T] = Either[ValidationError, T]

  case class ScriptParseError(m: String) extends ValidationError
}
