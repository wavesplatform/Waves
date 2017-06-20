package scorex.transaction

import scorex.account.{Account, Alias}
import scorex.block.{Block, MicroBlock}
import scorex.transaction.assets.exchange.Order

trait ValidationError

object ValidationError {

  case object InvalidAddress extends ValidationError
  case object NegativeAmount extends ValidationError
  case object InsufficientFee extends ValidationError
  case object TooBigArray extends ValidationError
  case object InvalidName extends ValidationError
  case object OverflowError extends ValidationError
  case object ToSelf extends ValidationError
  case object MissingSenderPrivateKey extends ValidationError
  case object UnsupportedTransactionType extends ValidationError
  case object InvalidRequestSignature extends ValidationError
  case class InvalidSignature(s: Signed, details: Option[InvalidSignature] = None) extends ValidationError
  case class AliasNotExists(a: Alias) extends ValidationError
  case class OrderValidationError(order: Order, err: String) extends ValidationError
  case class AccountBalanceError(errs: Map[Account, String]) extends ValidationError
  case class GenericError(err: String) extends ValidationError
  case class BlockAppendError(b: Block, err: String) extends ValidationError
  case class MicroBlockAppendError(microBlock: MicroBlock, err: String) extends ValidationError
  ???
}
