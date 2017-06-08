package scorex.transaction

import scorex.account.{Account, Alias}
import scorex.transaction.assets.exchange.Order

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
  case object MissingSenderPrivateKey extends ValidationError
  case class TransactionParameterValidationError(err: String) extends ValidationError
  case class CustomError(s: String) extends ValidationError
  case class UnsupportedTransactionType(tx: Transaction) extends ValidationError
  case class AliasNotExists(a: Alias) extends ValidationError
  case class TransactionValidationError(tx: Transaction, err: String) extends ValidationError
  case class OrderValidationError(order: Order, err: String) extends ValidationError
  case class AccountBalanceError(errs: Map[Account, String]) extends ValidationError
}
