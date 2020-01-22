package com.wavesplatform.transaction.smart

import cats.Id
import cats.implicits._
import com.google.common.base.Throwables
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.features.EstimatorProvider._
import com.wavesplatform.lang.{ExecutionError, ValidationError}
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.TermPrinter
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FALSE, TRUE}
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.metrics._
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError.{GenericError, HasScriptType, ScriptExecutionError, TransactionNotAllowedByScript}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.smart.script.trace.{AccountVerifierTrace, AssetVerifierTrace, TraceStep, TracedResult}
import com.wavesplatform.transaction.smart.script.ScriptRunner
import com.wavesplatform.transaction.smart.script.ScriptRunner.TxOrd
import com.wavesplatform.utils.ScorexLogging
import org.msgpack.core.annotations.VisibleForTesting
import shapeless.Coproduct

import scala.util.{Failure, Success, Try}

object Verifier extends ScorexLogging {

  private val stats = TxProcessingStats

  import stats.TxTimerExt

  type ValidationResult[T] = Either[ValidationError, T]

  def apply(blockchain: Blockchain, currentBlockHeight: Int)(tx: Transaction): TracedResult[ValidationError, Transaction] = {
    val validatedTx: TracedResult[ValidationError, Transaction] = tx match {
      case _: GenesisTransaction => Right(tx)
      case pt: ProvenTransaction =>
        (pt, blockchain.accountScript(pt.sender)) match {
          case (stx: SignedTransaction, None) =>
            stats.signatureVerification
              .measureForType(stx.builder.typeId)(stx.signaturesValid())
          case (et: ExchangeTransaction, scriptOpt) =>
            verifyExchange(et, blockchain, scriptOpt, currentBlockHeight)
          case (_: SignedTransaction, Some(_)) =>
            Left(GenericError("Can't process transaction with signature from scripted account"))
          case (_, Some(script)) =>
            stats.accountScriptExecution
              .measureForType(pt.builder.typeId)(verifyTx(blockchain, script, currentBlockHeight, pt, None))
          case _ =>
            stats.signatureVerification
              .measureForType(tx.builder.typeId)(verifyAsEllipticCurveSignature(pt))
        }
    }
    validatedTx.flatMap { tx =>
      tx.checkedAssets()
        .flatMap {
          case asset: IssuedAsset => blockchain.assetDescription(asset).flatMap(_.script).map(script => (script, asset))
          case _ => None
        }
        .foldRight(TracedResult(tx.asRight[ValidationError])) { (assetInfo, txr) =>
          txr.flatMap(tx =>
            stats.assetScriptExecution
              .measureForType(tx.builder.typeId)(verifyTx(blockchain, assetInfo._1, currentBlockHeight, tx, Some(assetInfo._2.id))))
        }
    }
  }

  private def logIfNecessary(
                              result: Either[ValidationError, _],
                              id:     String,
                              eval:   (Log[Id], Either[ExecutionError, EVALUATED])
                            ): Unit =
    result match {
      case Left(_) if log.logger.isDebugEnabled => log.debug(buildLogs(id, eval))
      case _       if log.logger.isTraceEnabled => log.trace(buildLogs(id, eval))
      case _                                    => ()
    }

  def verifyTx(blockchain: Blockchain,
               script: Script,
               height: Int,
               transaction: Transaction,
               assetIdOpt: Option[ByteStr]): TracedResult[ValidationError, Transaction] = {

    val isAsset       = assetIdOpt.nonEmpty
    val senderAddress = transaction.asInstanceOf[Authorized].sender.toAddress

    val txE = Try {
        val containerAddress = assetIdOpt.getOrElse(senderAddress.bytes)
        val eval = ScriptRunner(height, Coproduct[TxOrd](transaction), blockchain, script, isAsset, containerAddress)
        val scriptResult = eval match {
          case (log, Left(execError)) => Left(ScriptExecutionError(execError, log, isAsset))
          case (log, Right(FALSE))    => Left(TransactionNotAllowedByScript(log, isAsset))
          case (_,   Right(TRUE))     => Right(transaction)
          case (_,   Right(x))        => Left(GenericError(s"Script returned not a boolean result, but $x"))
        }
        val logId = s"transaction ${transaction.id()}"
        logIfNecessary(scriptResult, logId, eval)
        scriptResult
      } match {
        case Failure(e) =>
          Left(ScriptExecutionError(s"Uncaught execution error: ${Throwables.getStackTraceAsString(e)}", List.empty, isAsset))
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

  def verifyOrder(blockchain: Blockchain, script: Script, height: Int, order: Order): ValidationResult[Order] =
    for {
      _      <- Script.estimate(script, blockchain.estimator).leftMap(GenericError(_))
      result <- Try {
        val eval = ScriptRunner(height, Coproduct[ScriptRunner.TxOrd](order), blockchain, script, isAssetScript = false, order.sender.toAddress.bytes)
        val scriptResult = eval match {
          case (log, Left(execError)) => Left(ScriptExecutionError(execError, log, isAssetScript = false))
          case (log, Right(FALSE)) => Left(TransactionNotAllowedByScript(log, isAssetScript = false))
          case (_, Right(TRUE)) => Right(order)
          case (_, Right(x)) => Left(GenericError(s"Script returned not a boolean result, but $x"))
        }
        val logId = s"order ${order.idStr()}"
        logIfNecessary(scriptResult, logId, eval)
        scriptResult
      } match {
        case Failure(e) => Left(ScriptExecutionError(s"Uncaught execution error: $e", List.empty, isAssetScript = false))
        case Success(s) => s
      }
    } yield result

  def verifyExchange(et: ExchangeTransaction,
                     blockchain: Blockchain,
                     matcherScriptOpt: Option[Script],
                     height: Int): TracedResult[ValidationError, Transaction] = {

    val typeId    = et.builder.typeId
    val sellOrder = et.sellOrder
    val buyOrder  = et.buyOrder

    def matcherTxVerification: TracedResult[ValidationError, Transaction] =
      matcherScriptOpt
        .map { script =>
          if (et.version != 1) {
            stats.accountScriptExecution
              .measureForType(typeId)(verifyTx(blockchain, script, height, et, None))
          } else {
            TracedResult(Left(GenericError("Can't process transaction with signature from scripted account")))
          }
        }
        .getOrElse(stats.signatureVerification.measureForType(typeId)(verifyAsEllipticCurveSignature(et)))

    def orderVerification(order: Order): TracedResult[ValidationError, Order] = {
      val verificationResult = blockchain
        .accountScript(order.sender.toAddress)
        .map { script =>
          if (order.version != 1) {
            stats.orderValidation.measure(verifyOrder(blockchain, script, height, order))
          } else {
            Left(GenericError("Can't process order with signature from scripted account"))
          }
        }
        .getOrElse(stats.signatureVerification.measureForType(typeId)(verifyAsEllipticCurveSignature(order)))

      TracedResult(verificationResult)
    }

    def assetVerification(assetId: Asset): TracedResult[ValidationError, Transaction] = assetId match {
      case Waves => Right(et)
      case asset: IssuedAsset =>
        blockchain.assetScript(asset).fold[TracedResult[ValidationError, Transaction]](Right(et)) { script =>
          verifyTx(blockchain, script, height, et, assetId.compatId) leftMap {
            case x: HasScriptType => x
            case GenericError(x)  => ScriptExecutionError(x, List.empty, isAssetScript = true)
            case x                => ScriptExecutionError(x.toString, List.empty, isAssetScript = true)
          }
        }
    }

    lazy val txAssetsVerification: TracedResult[ValidationError, Transaction] = {
      Set(
        buyOrder.assetPair.amountAsset,
        buyOrder.assetPair.priceAsset,
        buyOrder.matcherFeeAssetId,
        sellOrder.matcherFeeAssetId
      ).foldLeft[TracedResult[ValidationError, Transaction]](Right(et)) {
        case (verificationResult, asset) => verificationResult flatMap (_ => assetVerification(asset))
      }
    }

    val estimate: TracedResult[GenericError, Option[Long]] =
      matcherScriptOpt.traverse(Script.estimate(_, blockchain.estimator).leftMap(GenericError(_)))

    for {
      _ <- estimate
      _ <- matcherTxVerification
      _ <- orderVerification(sellOrder)
      _ <- orderVerification(buyOrder)
      _ <- txAssetsVerification
    } yield et
  }

  def verifyAsEllipticCurveSignature[T <: Proven with Authorized](pt: T): Either[GenericError, T] =
    pt.proofs.proofs match {
      case p :: Nil =>
        Either.cond(crypto.verify(p.arr, pt.bodyBytes(), pt.sender),
                    pt,
                    GenericError(s"Proof doesn't validate as signature for $pt"))
      case _ => Left(GenericError("Transactions from non-scripted accounts must have exactly 1 proof"))
    }

  @VisibleForTesting
  private[smart] def buildLogs(
                                id:     String,
                                result: (Log[Id], Either[String, EVALUATED])
                              ): String = {
    val (execLog, execResult) = result
    val builder = new StringBuilder(s"Script for $id evaluated to $execResult")
    execLog
      .foldLeft(builder) {
        case (sb, (k, Right(v))) => sb.append(s"\nEvaluated `$k` to ")
          v match {
            case obj: EVALUATED => TermPrinter.print(str => sb.append(str), obj); sb
            case a              => sb.append(a.toString)
          }
        case (sb, (k, Left(err))) => sb.append(s"\nFailed to evaluate `$k`: $err")
      }
      .toString
  }
}
