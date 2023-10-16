package com.wavesplatform.transaction

import cats.Id
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.ContractLimits.FailFreeInvokeComplexity
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.state.InvokeScriptResult
import com.wavesplatform.transaction.TxValidationError.FailedTransactionError.Cause
import com.wavesplatform.transaction.assets.exchange.Order

object TxValidationError {
  type Validation[T] = Either[ValidationError, T]

  case class InvalidAddress(reason: String)                  extends ValidationError
  case class NegativeAmount(amount: Long, of: String)        extends ValidationError
  case class NonPositiveAmount(amount: Long, of: String)     extends ValidationError
  case class InvalidDecimals(decimals: Byte)                 extends ValidationError
  case class NegativeMinFee(minFee: Long, of: String)        extends ValidationError
  case object InsufficientFee                                extends ValidationError
  case object TooBigArray                                    extends ValidationError
  case class TooBigInBytes(err: String)                      extends ValidationError
  case object InvalidName                                    extends ValidationError
  case object InvalidAssetId                                 extends ValidationError
  case object OverflowError                                  extends ValidationError
  case object ToSelf                                         extends ValidationError
  case object MissingSenderPrivateKey                        extends ValidationError
  case object UnsupportedTransactionType                     extends ValidationError
  case object InvalidRequestSignature                        extends ValidationError
  case class BlockFromFuture(ts: Long)                       extends ValidationError
  case class AlreadyInTheState(txId: ByteStr, txHeight: Int) extends ValidationError
  case class AccountBalanceError(errs: Map[Address, String]) extends ValidationError
  case class AliasDoesNotExist(a: Alias)                     extends ValidationError { override def toString: String = s"Alias '$a' does not exist." }
  case class AliasIsDisabled(a: Alias)                       extends ValidationError
  case class OrderValidationError(order: Order, err: String) extends ValidationError
  case class SenderIsBlacklisted(addr: String)               extends ValidationError
  case class Mistiming(err: String)                          extends ValidationError
  case class BlockAppendError(err: String, b: Block)         extends ValidationError
  case class ActivationError(err: String)                    extends ValidationError
  case class GenericError(err: String)                       extends ValidationError

  object GenericError {
    def apply(ex: Throwable): GenericError = new GenericError(ex.getMessage)
  }

  case class InvalidSignature(entity: Signed, details: Option[InvalidSignature] = None) extends ValidationError {
    override def toString: String = s"InvalidSignature(${entity.toString + " reason: " + details})"
  }

  case class InvalidStateHash(blockStateHash: Option[ByteStr]) extends ValidationError

  sealed trait WithLog extends Product with Serializable {
    def log: Log[Id]
    def toStringWithLog(limit: Int): String
  }

  /** Errors which can produce failed transaction */
  case class FailedTransactionError(
      cause: Cause,
      spentComplexity: Long,
      log: Log[Id],
      error: Option[String],
      assetId: Option[ByteStr] = None,
      invocations: Seq[InvokeScriptResult.Invocation] = Nil
  ) extends ValidationError
      with WithLog {
    import FailedTransactionError.*

    def code: Int = cause.code
    def message: String = cause match {
      case Cause.DAppExecution | Cause.FeeForActions     => error.get
      case Cause.AssetScriptInAction | Cause.AssetScript => assetScriptError(assetId.get, error)
    }

    def isDAppExecution: Boolean  = assetId.isEmpty && error.nonEmpty
    def isAssetExecution: Boolean = assetId.nonEmpty && error.nonEmpty
    def isFailFree: Boolean       = spentComplexity <= FailFreeInvokeComplexity

    def addComplexity(complexity: Long): FailedTransactionError = copy(spentComplexity = spentComplexity + complexity)

    def withLog(log: Log[Id]): FailedTransactionError = copy(log = log)

    private def assetScriptError(assetId: ByteStr, error: Option[String]): String =
      s"Transaction is not allowed by script of the asset $assetId" + error.fold("")(e => s": $e")

    def errorDetails: String = s"FailedTransactionError(code = ${cause.code}, error = $message, log = "

    override def toString: String =
      if (message.startsWith("FailedTransactionError"))
        message
      else
        s"FailedTransactionError(code = ${cause.code}, error = $message)"

    override def toStringWithLog(limit: Int): String =
      s"$errorDetails${ErrorWithLogPrinter.logToString(log, limit)})"
  }

  object FailedTransactionError {
    def dAppExecution(error: String, spentComplexity: Long, log: Log[Id] = List.empty): FailedTransactionError =
      FailedTransactionError(Cause.DAppExecution, spentComplexity, log, Some(error), None)

    def feeForActions(error: String, spentComplexity: Long, log: Log[Id]): FailedTransactionError =
      FailedTransactionError(Cause.FeeForActions, spentComplexity, log, Some(error), None)

    def assetExecutionInAction(error: String, spentComplexity: Long, log: Log[Id], assetId: ByteStr): FailedTransactionError =
      FailedTransactionError(Cause.AssetScriptInAction, spentComplexity, log, Some(error), Some(assetId))

    def notAllowedByAssetInAction(spentComplexity: Long, log: Log[Id], assetId: ByteStr): FailedTransactionError =
      FailedTransactionError(Cause.AssetScriptInAction, spentComplexity, log, None, Some(assetId))

    def assetExecution(error: String, spentComplexity: Long, log: Log[Id], assetId: ByteStr): FailedTransactionError =
      FailedTransactionError(Cause.AssetScript, spentComplexity, log, Some(error), Some(assetId))

    def notAllowedByAsset(spentComplexity: Long, log: Log[Id], assetId: ByteStr): FailedTransactionError =
      FailedTransactionError(Cause.AssetScript, spentComplexity, log, None, Some(assetId))

    def asFailedScriptError(ve: ValidationError): FailedTransactionError =
      ve match {
        case fte: FailedTransactionError => fte
        case GenericError(err)           => this.dAppExecution(err, spentComplexity = 0L)
        case err                         => this.dAppExecution(err.toString, spentComplexity = 0L)
      }

    sealed trait Cause extends Product with Serializable {
      def code: Int
    }
    object Cause {
      case object DAppExecution extends Cause {
        override def code: Int = 1
      }
      case object FeeForActions extends Cause {
        override def code: Int = 2
      }
      case object AssetScriptInAction extends Cause {
        override def code: Int = 3
      }
      case object AssetScript extends Cause {
        override def code: Int = 4
      }
    }
  }

  case class ScriptExecutionError(message: String, log: Log[Id], assetId: Option[ByteStr]) extends ValidationError with WithLog {
    def isAssetScript: Boolean = assetId.isDefined
    private val target: String = assetId.fold("Account")(_ => "Asset")
    override def toString: String =
      if (String.valueOf(message).startsWith("ScriptExecutionError"))
        message
      else
        s"ScriptExecutionError(error = $message, type = $target)"

    override def toStringWithLog(limit: Int): String =
      s"ScriptExecutionError(error = $message, type = $target, log = ${ErrorWithLogPrinter.logToString(log, limit)})"
  }

  case class InvokeRejectError(message: String, log: Log[Id]) extends ValidationError with WithLog {
    override def toString: String = s"InvokeRejectError(error = $message)"

    override def toStringWithLog(limit: Int): String =
      s"InvokeRejectError(error = $message, log = ${ErrorWithLogPrinter.logToString(log, limit)})"
  }

  case class TransactionNotAllowedByScript(log: Log[Id], assetId: Option[ByteStr]) extends ValidationError with WithLog {
    def isAssetScript: Boolean    = assetId.isDefined
    private val target: String    = assetId.fold("Account")(_ => "Asset")
    override def toString: String = s"TransactionNotAllowedByScript(type = $target)"

    override def toStringWithLog(limit: Int): String =
      s"TransactionNotAllowedByScript(type = $target, log = ${ErrorWithLogPrinter.logToString(log, limit)})"
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
