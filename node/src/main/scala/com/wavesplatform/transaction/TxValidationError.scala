package com.wavesplatform.transaction

import cats.Id
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.transaction.assets.exchange.Order

import scala.util.Either

object TxValidationError {
  type Validation[T] = Either[ValidationError, T]

  case class InvalidAddress(reason: String)                    extends ValidationError
  case class NegativeAmount(amount: Long, of: String)          extends ValidationError
  case class NonPositiveAmount(amount: Long, of: String)       extends ValidationError
  case class NegativeMinFee(minFee: Long, of: String)          extends ValidationError
  case class InsufficientFee(msg: String = "insufficient fee") extends ValidationError
  case object TooBigArray                                      extends ValidationError
  case object InvalidName                                      extends ValidationError
  case object InvalidAssetId                                   extends ValidationError
  case object OverflowError                                    extends ValidationError
  case object ToSelf                                           extends ValidationError
  case object MissingSenderPrivateKey                          extends ValidationError
  case object UnsupportedTransactionType                       extends ValidationError
  case object InvalidRequestSignature                          extends ValidationError
  case class BlockFromFuture(ts: Long)                         extends ValidationError
  case class AlreadyInTheState(txId: ByteStr, txHeight: Int)   extends ValidationError
  case class AccountBalanceError(errs: Map[Address, String])   extends ValidationError
  case class AliasDoesNotExist(a: Alias)                       extends ValidationError { override def toString: String = s"Alias '$a' does not exists." }
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

  /** Errors which can produce failed transaction */
  sealed trait FailedTransactionError extends Product with Serializable with ValidationError {
    import FailedTransactionError._

    def cause: Cause = this match {
      case e: ScriptExecutionError.ByDAppScript                   => Cause(1, e.error)
      case e: InsufficientInvokeActionFee                         => Cause(2, e.error)
      case e: ScriptExecutionError.ByAssetScriptInAction          => Cause(3, assetScriptError(e.assetId, Some(e.error)))
      case e: TransactionNotAllowedByScript.ByAssetScriptInAction => Cause(3, assetScriptError(e.assetId, None))
      case e: ScriptExecutionError.ByAssetScript                  => Cause(4, assetScriptError(e.assetId, Some(e.error)))
      case e: TransactionNotAllowedByScript.ByAssetScript         => Cause(4, assetScriptError(e.assetId, None))
    }

    private def assetScriptError(assetId: ByteStr, error: Option[String]): String =
      s"Transaction is not allowed by script of the asset $assetId" + error.fold("")(e => s": $e")
  }
  sealed trait FailedExecutionError extends ScriptExecutionError with FailedTransactionError
  sealed trait FailedResultError    extends TransactionNotAllowedByScript with FailedTransactionError

  object FailedTransactionError {
    case class Cause(code: Int, error: String)
  }

  sealed trait ScriptExecutionError extends ValidationError {
    import ScriptExecutionError._

    def error: String
    def log: Log[Id]
    val isAssetScript: Boolean = this match {
      case _: ByAssetScript         => true
      case _: ByAssetScriptInAction => true
      case _: ByAccountScript       => false
      case _: ByDAppScript          => false
    }

    private val target: String = this match {
      case _: ByAssetScript         => "Asset"
      case _: ByAssetScriptInAction => "Asset"
      case _: ByAccountScript       => "Account"
      case _: ByDAppScript          => "DApp"
    }

    override def toString: String = s"ScriptExecutionError(error = $error, type = $target, log =${logToString(log)})"
  }

  object ScriptExecutionError {
    def apply(error: String, log: Log[Id], assetId: Option[ByteStr]): ScriptExecutionError =
      assetId.fold[ScriptExecutionError](ByAccountScript(error, log))(ai => ByAssetScript(error, log, ai))

    final case class ByAccountScript(error: String, log: Log[Id])                         extends ScriptExecutionError
    final case class ByDAppScript(error: String, log: Log[Id] = List.empty)               extends FailedExecutionError
    final case class ByAssetScriptInAction(error: String, log: Log[Id], assetId: ByteStr) extends FailedExecutionError
    final case class ByAssetScript(error: String, log: Log[Id], assetId: ByteStr)         extends FailedExecutionError
  }

  case class InsufficientInvokeActionFee(error: String) extends ValidationError with FailedTransactionError {
    override def toString: String = s"InsufficientInvokeActionFee(error = $error)"
  }

  sealed trait TransactionNotAllowedByScript extends ValidationError {
    import TransactionNotAllowedByScript._

    def log: Log[Id]
    val isAssetScript: Boolean = this match {
      case _: ByAssetScript         => true
      case _: ByAssetScriptInAction => true
      case _: ByAccountScript       => false
    }

    private val target: String = this match {
      case _: ByAssetScript         => "Asset"
      case _: ByAssetScriptInAction => "Asset"
      case _: ByAccountScript       => "Account"
    }

    override def toString: String = s"TransactionNotAllowedByScript(type = $target, log =${logToString(log)})"
  }

  object TransactionNotAllowedByScript {
    def apply(log: Log[Id], assetId: Option[ByteStr]): TransactionNotAllowedByScript =
      assetId.fold[TransactionNotAllowedByScript](ByAccountScript(log))(ai => ByAssetScript(log, ai))

    final case class ByAccountScript(log: Log[Id])                         extends TransactionNotAllowedByScript
    final case class ByAssetScriptInAction(log: Log[Id], assetId: ByteStr) extends FailedResultError
    final case class ByAssetScript(log: Log[Id], assetId: ByteStr)         extends FailedResultError
  }

  def logToString(log: Log[Id]): String =
    if (log.isEmpty) ""
    else {
      log
        .map {
          case (name, Right(v))    => s"$name = ${v.prettyString(1)}"
          case (name, l @ Left(_)) => s"$name = $l"
        }
        .map("\t" + _)
        .mkString("\n", "\n", "\n")
    }

  case class MicroBlockAppendError(err: String, microBlock: MicroBlock) extends ValidationError {
    override def toString: String = s"MicroBlockAppendError($err, ${microBlock.totalResBlockSig} ~> ${microBlock.reference.trim}])"
  }

  case object EmptyDataKey extends ValidationError {
    override def toString: String = "Empty key found"
  }

  case object DuplicatedDataKeys extends ValidationError {
    override def toString: String = s"Duplicated keys found"
  }

  case class WrongChain(expected: Byte, provided: Byte) extends ValidationError {
    override def toString: String = s"Wrong chain-id. Expected - $expected, provided - $provided"
  }

  case class UnsupportedTypeAndVersion(typeId: Byte, version: Int) extends ValidationError {
    override def toString: String = s"Bad transaction type ($typeId) and version ($version)"
  }

  case class UsupportedProofVersion(version: Int, supported: List[Int]) extends ValidationError {
    override def toString: String = s"Unsupported proofs version - $version. Expected one of ${supported.mkString("[", ", ", "]")}"
  }

  case class TooManyProofs(max: Int, actual: Int) extends ValidationError {
    override def toString: String = s"Too many proofs ($actual), only $max allowed"
  }

  case class ToBigProof(max: Int, actual: Int) extends ValidationError {
    override def toString: String = s"Too large proof ($actual), must be max $max bytes"
  }
}
