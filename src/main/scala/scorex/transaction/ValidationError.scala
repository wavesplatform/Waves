package scorex.transaction


sealed trait ValidationError {

}

object ValidationError {
  case object InvalidAddress extends ValidationError
  case object NegativeAmount extends ValidationError
  case object InsufficientFee extends ValidationError
  case object NoBalance extends ValidationError
  case object TooBigArray extends ValidationError
  case object InvalidSignature extends ValidationError
  case object InvalidName extends ValidationError
  case object StateCheckFailed extends ValidationError
  case object OverflowError extends ValidationError

  case class CustomValidationError(err: String) extends ValidationError

}
