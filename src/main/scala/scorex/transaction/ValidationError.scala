package scorex.transaction

sealed trait ValidationError

object ValidationError {
  case object InvalidAddress extends ValidationError
  case object NegativeAmount extends ValidationError
  case object InsufficientFee extends ValidationError
  case object TooBigArray extends ValidationError
  case object InvalidSignature extends ValidationError
  case object InvalidName extends ValidationError
  case object OverflowError extends ValidationError
  case object ToSelf extends ValidationError

  case class TransactionParameterValidationError(err: String) extends ValidationError

  case class TransactionValidationError(ts: Transaction, err: String) extends ValidationError

}
