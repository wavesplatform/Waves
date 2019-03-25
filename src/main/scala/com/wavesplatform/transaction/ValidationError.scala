package com.wavesplatform.transaction

import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.transaction.assets.exchange.Order

import scala.util.Either

trait ValidationError extends Product with Serializable

object ValidationError {
  type Validation[T] = Either[ValidationError, T]

  case class InvalidAddress(reason: String)                    extends ValidationError
  case class NegativeAmount(amount: Long, of: String)          extends ValidationError
  case class NonPositiveAmount(amount: Long, of: String)       extends ValidationError
  case class NegativeMinFee(minFee: Long, of: String)          extends ValidationError
  case class InsufficientFee(msg: String = "insufficient fee") extends ValidationError
  case object TooBigArray                                      extends ValidationError
  case object InvalidName                                      extends ValidationError
  case object OverflowError                                    extends ValidationError
  case object ToSelf                                           extends ValidationError
  case object MissingSenderPrivateKey                          extends ValidationError
  case object UnsupportedTransactionType                       extends ValidationError
  case object InvalidRequestSignature                          extends ValidationError
  case class BlockFromFuture(ts: Long)                         extends ValidationError
  case class ScriptParseError(m: String)                       extends ValidationError
  case class AlreadyInTheState(txId: ByteStr, txHeight: Int)   extends ValidationError
  case class AccountBalanceError(errs: Map[Address, String])   extends ValidationError
  case class AliasDoesNotExist(a: Alias)                       extends ValidationError
  case class AliasIsDisabled(a: Alias)                         extends ValidationError
  case class OrderValidationError(order: Order, err: String)   extends ValidationError
  case class SenderIsBlacklisted(addr: String)                 extends ValidationError
  case class Mistiming(err: String)                            extends ValidationError
  case class BlockAppendError(err: String, b: Block)           extends ValidationError
  case class ActivationError(err: String)                      extends ValidationError
  case class UnsupportedVersion(version: Int)                  extends ValidationError
  case class GenericError(err: String)                         extends ValidationError

  object GenericError {
    def apply(ex: Throwable): GenericError = new GenericError(ex.getMessage)
  }

  case class InvalidSignature(s: Signed, details: Option[InvalidSignature] = None) extends ValidationError {
    override def toString: String = s"InvalidSignature(${s.toString + " reason: " + details})"
  }

  trait HasScriptType extends ValidationError {
    def isTokenScript: Boolean
  }

  case class ScriptExecutionError(error: String, log: Log, isTokenScript: Boolean) extends ValidationError with HasScriptType

  case class TransactionNotAllowedByScript(log: Log, isTokenScript: Boolean) extends ValidationError with HasScriptType

  case class MicroBlockAppendError(err: String, microBlock: MicroBlock) extends ValidationError {
    override def toString: String = s"MicroBlockAppendError($err, ${microBlock.totalResBlockSig} ~> ${microBlock.prevResBlockSig.trim}])"
  }

}
