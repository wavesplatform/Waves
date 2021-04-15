package com.wavesplatform.transaction

import scala.util.Either

import cats.Id
import com.wavesplatform.account.{Address, Alias}
import com.wavesplatform.block.{Block, MicroBlock}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.state.InvokeScriptResult
import com.wavesplatform.transaction.TxValidationError.FailedTransactionError.Cause
import com.wavesplatform.transaction.assets.exchange.Order

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

  case class InvalidSignature(entity: Signed, details: Option[InvalidSignature] = None) extends ValidationError {
    override def toString: String = s"InvalidSignature(${entity.toString + " reason: " + details})"
  }

  sealed trait WithLog extends Product with Serializable {
    def log: Log[Id]
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
    import FailedTransactionError._

    def code: Int = cause.code
    def message: String = cause match {
      case Cause.DAppExecution | Cause.FeeForActions     => error.get
      case Cause.AssetScriptInAction | Cause.AssetScript => assetScriptError(assetId.get, error)
    }

    def isAssetScript: Boolean    = assetId.isDefined
    def isExecutionError: Boolean = error.nonEmpty

    def addComplexity(complexity: Long): FailedTransactionError = copy(spentComplexity = spentComplexity + complexity)

    private def assetScriptError(assetId: ByteStr, error: Option[String]): String =
      s"Transaction is not allowed by script of the asset $assetId" + error.fold("")(e => s": $e")

    override def toString: String =
      if (message.startsWith("FailedTransactionError"))
        message
      else
        s"FailedTransactionError(code = ${cause.code}, error = $message, log =${logToString(log)})"
  }

  object FailedTransactionError {
    def dAppExecution(error: String, spentComplexity: Long, log: Log[Id] = List.empty): FailedTransactionError =
      FailedTransactionError(Cause.DAppExecution, spentComplexity, log, Some(error), None)

    def feeForActions(error: String, spentComplexity: Long): FailedTransactionError =
      FailedTransactionError(Cause.FeeForActions, spentComplexity, List.empty, Some(error), None)

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

  case class ScriptExecutionError(error: String, log: Log[Id], assetId: Option[ByteStr]) extends ValidationError with WithLog {
    def isAssetScript: Boolean = assetId.isDefined
    private val target: String = assetId.fold("Account")(_ => "Asset")
    override def toString: String =
      if (error.startsWith("ScriptExecutionError"))
        error
      else
        s"ScriptExecutionError(error = $error, type = $target, log =${logToString(log)})"
  }

  object ScriptExecutionError {
    def dAppExecution(error: String, log: Log[Id]): ScriptExecutionError = ScriptExecutionError(error, log, None)
  }

  case class TransactionNotAllowedByScript(log: Log[Id], assetId: Option[ByteStr]) extends ValidationError {
    def isAssetScript: Boolean    = assetId.isDefined
    private val target: String    = assetId.fold("Account")(_ => "Asset")
    override def toString: String = s"TransactionNotAllowedByScript(type = $target, log =${logToString(log)})"
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
