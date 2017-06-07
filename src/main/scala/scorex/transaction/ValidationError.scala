package scorex.transaction

import scorex.account.{Account, Alias}

sealed trait ValidationError

sealed trait ValidationErrorByAccount extends ValidationError {
  val acc: Account
}

sealed trait StateValidationError extends ValidationError

object ValidationError {

  case object InvalidAddress extends ValidationError
  case object NegativeAmount extends ValidationError
  case object InsufficientFee extends ValidationError
  case object TooBigArray extends ValidationError
  case object InvalidSignature extends ValidationError
  case object InvalidName extends ValidationError
  case object OverflowError extends ValidationError
  case object ToSelf extends ValidationError
  case object MissingSenderPrivateKey extends ValidationError
  case class TransactionParameterValidationError(err: String) extends ValidationError
  case class CustomError(s: String) extends ValidationError

  case class UnsupportedTransactionType(tx:Transaction) extends ValidationError
  case class AliasNotExists(a : Alias) extends StateValidationError
  case class TransactionValidationError(tx: Transaction, err: String) extends StateValidationError
  case class AccountValidationError(acc: Account, err: String) extends ValidationErrorByAccount
  case class TransactionValidationErrorByAccount(tx: Transaction, acc: Account, err: String) extends ValidationErrorByAccount
}
