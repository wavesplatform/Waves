package com.wavesplatform.transaction.smart

import cats.Id
import cats.implicits._
import com.google.common.base.Throwables
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.ContractLimits
import com.wavesplatform.lang.v1.compiler.TermPrinter
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FALSE, TRUE}
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.traits.domain.Recipient
import com.wavesplatform.lang.{ExecutionError, ValidationError}
import com.wavesplatform.metrics._
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.{GenericError, ScriptExecutionError, TransactionNotAllowedByScript}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.transaction.smart.script.trace.{AccountVerifierTrace, AssetVerifierTrace, TraceStep, TracedResult}
import com.wavesplatform.utils.ScorexLogging
import org.msgpack.core.annotations.VisibleForTesting
import shapeless.Coproduct

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Verifier extends ScorexLogging {

  private val stats = TxProcessingStats

  import stats.TxTimerExt

  type ValidationResult[T] = Either[ValidationError, T]

  def apply(blockchain: Blockchain, limitedExecution: Boolean = false)(tx: Transaction): TracedResult[ValidationError, Transaction] = tx match {
    case _: GenesisTransaction => Right(tx)
    case pt: ProvenTransaction =>
      (pt, blockchain.accountScript(pt.sender.toAddress).map(_.script)) match {
        case (stx: SignedTransaction, None) =>
          stats.signatureVerification
            .measureForType(stx.typeId)(stx.signaturesValid())
        case (et: ExchangeTransaction, scriptOpt) =>
          verifyExchange(et, blockchain, scriptOpt, if (limitedExecution) ContractLimits.FailFreeInvokeComplexity else Int.MaxValue)
        case (tx: SigProofsSwitch, Some(_)) if tx.usesLegacySignature =>
          Left(GenericError("Can't process transaction with signature from scripted account"))
        case (_: SignedTransaction, Some(_)) =>
          Left(GenericError("Can't process transaction with signature from scripted account"))
        case (_, Some(script)) =>
          stats.accountScriptExecution
            .measureForType(pt.typeId)(verifyTx(blockchain, script, pt, None))
        case _ =>
          stats.signatureVerification
            .measureForType(tx.typeId)(verifyAsEllipticCurveSignature(pt))
      }
  }

  /** Verifies asset scripts and returns diff with complexity. In case of error returns spent complexity */
  def assets(blockchain: Blockchain, remainingComplexity: Int)(tx: Transaction): TracedResult[(Long, ValidationError), Diff] = {
    @tailrec
    def loop(
        assets: List[(AssetScriptInfo, IssuedAsset)],
        fullComplexity: Long,
        fullTrace: List[TraceStep]
    ): (Long, TracedResult[ValidationError, Transaction]) = {
      assets match {
        case (AssetScriptInfo(script, complexity), asset) :: remaining =>
          val complexityLimit =
            if (remainingComplexity == Int.MaxValue) remainingComplexity
            else remainingComplexity - fullComplexity.toInt

          val spentComplexity = fullComplexity + complexity
          stats.assetScriptExecution
            .measureForType(tx.typeId)(verifyTx(blockchain, script, tx, Some(asset.id), complexityLimit)) match {
            case TracedResult(e @ Left(_), trace) => (spentComplexity, TracedResult(e, fullTrace ::: trace))
            case TracedResult(Right(_), trace)    => loop(remaining, spentComplexity, fullTrace ::: trace)
          }
        case Nil => (fullComplexity, TracedResult(Right(tx), fullTrace))
      }
    }

    val assets = tx.checkedAssets.flatMap { asset =>
      blockchain.assetDescription(asset).flatMap(_.script).map(script => (script, asset))
    }.toList

    val additionalAssets = (tx match {
      case etx: ExchangeTransaction => List(etx.buyOrder.matcherFeeAssetId, etx.sellOrder.matcherFeeAssetId)
      case _                        => List.empty
    }).distinct.collect {
      case ia: IssuedAsset =>
        blockchain.assetDescription(ia).flatMap(_.script).map(script => (script, ia))
    }.flatten

    val (complexity, result)  = loop(assets, 0L, Nil)
    val (_, additionalResult) = loop(additionalAssets, 0L, Nil)

    result
      .flatMap(_ => additionalResult)
      .leftMap(ve => (complexity, ve))
      .as(Diff.empty.copy(scriptsComplexity = complexity))
  }

  private def logIfNecessary(
      result: Either[ValidationError, _],
      id: String,
      eval: (Log[Id], Either[ExecutionError, EVALUATED])
  ): Unit =
    result match {
      case Left(_) if log.logger.isDebugEnabled => log.debug(buildLogs(id, eval))
      case _ if log.logger.isTraceEnabled       => log.trace(buildLogs(id, eval))
      case _                                    => ()
    }

  private def verifyTx(
      blockchain: Blockchain,
      script: Script,
      transaction: Transaction,
      assetIdOpt: Option[ByteStr],
      complexityLimit: Int = Int.MaxValue
  ): TracedResult[ValidationError, Transaction] = {

    val isAsset       = assetIdOpt.nonEmpty
    val senderAddress = transaction.asInstanceOf[Authorized].sender.toAddress

    val txE = Try {
      val containerAddress = assetIdOpt.fold(Coproduct[Environment.Tthis](Recipient.Address(ByteStr(senderAddress.bytes))))(
        v => Coproduct[Environment.Tthis](Environment.AssetId(v.arr))
      )
      val eval = ScriptRunner(Coproduct[TxOrd](transaction), blockchain, script, isAsset, containerAddress, complexityLimit)
      val scriptResult = eval match {
        case (log, Left(execError)) => Left(ScriptExecutionError(execError, log, assetIdOpt))
        case (log, Right(FALSE))    => Left(TransactionNotAllowedByScript(log, assetIdOpt))
        case (_, Right(TRUE))       => Right(transaction)
        case (log, Right(x))        => Left(ScriptExecutionError(s"Script returned not a boolean result, but $x", log, assetIdOpt))
      }
      val logId = s"transaction ${transaction.id()}"
      logIfNecessary(scriptResult, logId, eval)
      scriptResult
    } match {
      case Failure(e) =>
        Left(ScriptExecutionError(s"Uncaught execution error: ${Throwables.getStackTraceAsString(e)}", List.empty, assetIdOpt))
      case Success(s) => s
    }
    val error2Trace: Option[ValidationError] => List[TraceStep] =
      e => {
        val trace = assetIdOpt.fold[TraceStep](
          AccountVerifierTrace(senderAddress, e)
        )(
          (id: ByteStr) => AssetVerifierTrace(id, e)
        )
        List(trace)
      }

    txE match {
      case r @ Right(_) => TracedResult(r, error2Trace(None))
      case l @ Left(e)  => TracedResult(l, error2Trace(Some(e)))
    }
  }

  private def verifyOrder(blockchain: Blockchain, script: Script, order: Order, complexityLimit: Int): ValidationResult[Order] =
    for {
      result <- Try {
        val eval =
          ScriptRunner(
            Coproduct[ScriptRunner.TxOrd](order),
            blockchain,
            script,
            isAssetScript = false,
            Coproduct[Environment.Tthis](Recipient.Address(ByteStr(order.sender.toAddress.bytes))),
            complexityLimit
          )
        val scriptResult = eval match {
          case (log, Left(execError)) => Left(ScriptExecutionError(execError, log, None))
          case (log, Right(FALSE))    => Left(TransactionNotAllowedByScript(log, None))
          case (_, Right(TRUE))       => Right(order)
          case (_, Right(x))          => Left(GenericError(s"Script returned not a boolean result, but $x"))
        }
        val logId = s"order ${order.idStr()}"
        logIfNecessary(scriptResult, logId, eval)
        scriptResult
      } match {
        case Failure(e) => Left(ScriptExecutionError(s"Uncaught execution error: $e", List.empty, None))
        case Success(s) => s
      }
    } yield result

  private def verifyExchange(
      et: ExchangeTransaction,
      blockchain: Blockchain,
      matcherScriptOpt: Option[Script],
      complexityLimit: Int
  ): TracedResult[ValidationError, Transaction] = {

    val typeId    = et.typeId
    val sellOrder = et.sellOrder
    val buyOrder  = et.buyOrder

    def matcherTxVerification: TracedResult[ValidationError, Transaction] =
      matcherScriptOpt
        .map { script =>
          if (et.version != 1) {
            stats.accountScriptExecution
              .measureForType(typeId)(verifyTx(blockchain, script, et, None, complexityLimit))
          } else {
            TracedResult(Left(GenericError("Can't process transaction with signature from scripted account")))
          }
        }
        .getOrElse(stats.signatureVerification.measureForType(typeId)(verifyAsEllipticCurveSignature(et)))

    def orderVerification(order: Order): TracedResult[ValidationError, Order] = {
      val verificationResult = blockchain
        .accountScript(order.sender.toAddress)
        .map { asi =>
          if (order.version != 1) {
            stats.orderValidation.withoutTags().measure(verifyOrder(blockchain, asi.script, order, complexityLimit))
          } else {
            Left(GenericError("Can't process order with signature from scripted account"))
          }
        }
        .getOrElse(stats.signatureVerification.measureForType(typeId)(verifyAsEllipticCurveSignature(order)))

      TracedResult(verificationResult)
    }

    for {
      _ <- matcherTxVerification
      _ <- orderVerification(sellOrder)
      _ <- orderVerification(buyOrder)
    } yield et
  }

  def verifyAsEllipticCurveSignature[T <: Proven with Authorized](pt: T): Either[GenericError, T] =
    pt.proofs.proofs match {
      case p +: Nil =>
        Either.cond(crypto.verify(p, pt.bodyBytes(), pt.sender), pt, GenericError(s"Proof doesn't validate as signature for $pt"))
      case _ => Left(GenericError("Transactions from non-scripted accounts must have exactly 1 proof"))
    }

  @VisibleForTesting
  private[smart] def buildLogs(
      id: String,
      result: (Log[Id], Either[String, EVALUATED])
  ): String = {
    val (execLog, execResult) = result
    val builder               = new StringBuilder(s"Script for $id evaluated to $execResult")
    execLog
      .foldLeft(builder) {
        case (sb, (k, Right(v))) =>
          sb.append(s"\nEvaluated `$k` to ")
          v match {
            case obj: EVALUATED => TermPrinter.print(str => sb.append(str), obj); sb
            case a              => sb.append(a.toString)
          }
        case (sb, (k, Left(err))) => sb.append(s"\nFailed to evaluate `$k`: $err")
      }
      .toString
  }
}
