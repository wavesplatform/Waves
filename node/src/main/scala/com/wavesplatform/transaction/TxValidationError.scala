package com.wavesplatform.transaction

import cats.Id
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.transaction.TxValidationError.CanFail.Reason
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

  trait HasScriptType extends ValidationError

  /** Marker trait for errors which can produce failed transaction */
  sealed trait CanFail extends Product with Serializable {
    def reason: Reason = Reason.Asset
    def error: String
  }

  object CanFail {
    sealed trait Reason extends Product with Serializable {
      def code: Int
    }
    object Reason {
      final case object DApp extends Reason {
        val code: Int = 1
      }
      final case object InsufficientFee extends Reason {
        val code: Int = 2
      }
      final case object AssetInAction extends Reason {
        val code: Int = 3
      }
      final case object Asset extends Reason {
        val code: Int = 4
      }
    }
  }

  sealed trait ScriptExecutionError extends ValidationError with HasScriptType {
    def error: String
    def log: Log[Id]
    def target: String

    override def toString: String = s"ScriptExecutionError(error = $error, type = $target, log =${logToString(log)})"
  }

  object ScriptExecutionError {
    def apply(error: String, log: Log[Id], isAssetScript: Boolean): ScriptExecutionError =
      if (isAssetScript) ByAssetScript(error, log, Reason.Asset) else ByAccountScript(error, log)

    def asset(error: String, log: Log[Id], reason: Reason): ScriptExecutionError = ByAssetScript(error, log, reason)
    def dApp(error: String, log: Log[Id]): ScriptExecutionError                  = ByDAppScript(error, log)

    def unapply(e: ScriptExecutionError): Option[(String, Log[Id], Boolean)] =
      e match {
        case ByAccountScript(error, log)  => Some((error, log, false))
        case ByDAppScript(error, log)     => Some((error, log, false))
        case ByAssetScript(error, log, _) => Some((error, log, true))
      }

    private final case class ByAccountScript(error: String, log: Log[Id]) extends ScriptExecutionError {
      override val target: String = "Account"
    }

    private final case class ByDAppScript(error: String, log: Log[Id]) extends ScriptExecutionError with CanFail {
      override val target: String = "DApp"
      override val reason: Reason = Reason.DApp
    }

    private final case class ByAssetScript(error: String, log: Log[Id], override val reason: Reason) extends ScriptExecutionError with CanFail {
      override val target: String = "Asset"
    }
  }

  case class InsufficientInvokeActionFee(error: String) extends ValidationError with CanFail {
    override def toString: String = s"ScriptExecutionError(error = $error)"
    override val reason: Reason   = Reason.InsufficientFee
  }

  sealed trait TransactionNotAllowedByScript extends ValidationError with HasScriptType {
    def log: Log[Id]
    def target: String

    override def toString: String = s"TransactionNotAllowedByScript(type = $target, log =${logToString(log)})"
  }

  object TransactionNotAllowedByScript {
    def apply(log: Log[Id], isAssetScript: Boolean): TransactionNotAllowedByScript =
      if (isAssetScript) ByAssetScript(log, Reason.Asset) else ByAccountScript(log)

    def asset(log: Log[Id], reason: Reason): TransactionNotAllowedByScript = ByAssetScript(log, reason)

    def unapply(e: TransactionNotAllowedByScript): Option[(Log[Id], Boolean)] =
      e match {
        case ByAccountScript(log)  => Some((log, false))
        case ByAssetScript(log, _) => Some((log, true))
      }

    private final case class ByAccountScript(log: Log[Id]) extends TransactionNotAllowedByScript {
      override val target: String = "Account"
    }

    private final case class ByAssetScript(log: Log[Id], override val reason: Reason) extends TransactionNotAllowedByScript with CanFail {
      override val target: String = "Asset"
      override def error: String  = "Transaction is not allowed by token-script"
    }
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
