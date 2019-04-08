package com.wavesplatform.transaction.smart

import cats.implicits._
import com.google.common.base.Throwables
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.crypto
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, FALSE, TRUE}
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.metrics._
import com.wavesplatform.state._
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.ValidationError.{GenericError, HasScriptType, ScriptExecutionError, TransactionNotAllowedByScript}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.smart.script.{Script, ScriptRunner}
import com.wavesplatform.utils.ScorexLogging
import shapeless.Coproduct

import scala.util.{Failure, Success, Try}

object Verifier extends ScorexLogging {

  private val stats = TxProcessingStats

  import stats.TxTimerExt

  type ValidationResult[T] = Either[ValidationError, T]

  def apply(blockchain: Blockchain, currentBlockHeight: Int)(tx: Transaction): ValidationResult[Transaction] =
    (tx match {
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
    }).flatMap { tx =>
      tx.checkedAssets()
        .flatMap {
          case asset: IssuedAsset => blockchain.assetDescription(asset).flatMap(_.script).map(script => (script, asset))
          case _                  => None
        }
        .foldRight(Either.right[ValidationError, Transaction](tx)) { (assetInfo, txr) =>
          txr.right.flatMap(tx =>
            stats.assetScriptExecution
              .measureForType(tx.builder.typeId)(verifyTx(blockchain, assetInfo._1, currentBlockHeight, tx, Some(assetInfo._2.id))))
        }
    }

  def verifyTx(blockchain: Blockchain,
               script: Script,
               height: Int,
               transaction: Transaction,
               assetIdOpt: Option[ByteStr]): ValidationResult[Transaction] =
    Try {
      val containerAddress = if (assetIdOpt.nonEmpty) assetIdOpt.get else transaction.asInstanceOf[Authorized].sender.toAddress.bytes
      logged(
        s"transaction ${transaction.id()}",
        ScriptRunner(height, Coproduct[ScriptRunner.TxOrd](transaction), blockchain, script, assetIdOpt.nonEmpty, containerAddress)
      ) match {
        case (log, Left(execError)) => Left(ScriptExecutionError(execError, log, assetIdOpt.nonEmpty))
        case (log, Right(FALSE)) =>
          Left(TransactionNotAllowedByScript(log, assetIdOpt.nonEmpty))
        case (_, Right(TRUE)) => Right(transaction)
        case (_, Right(x))    => Left(GenericError(s"Script returned not a boolean result, but $x"))
      }
    } match {
      case Failure(e) =>
        Left(ScriptExecutionError(s"Uncaught execution error: ${Throwables.getStackTraceAsString(e)}", List.empty, assetIdOpt.nonEmpty))
      case Success(s) => s
    }

  def verifyOrder(blockchain: Blockchain, script: Script, height: Int, order: Order): ValidationResult[Order] =
    Try {
      logged(
        s"order ${order.idStr()}",
        ScriptRunner(height, Coproduct[ScriptRunner.TxOrd](order), blockchain, script, isAssetScript = false, order.sender.toAddress.bytes)
      ) match {
        case (log, Left(execError)) => Left(ScriptExecutionError(execError, log, isAssetScript = false))
        case (log, Right(FALSE))    => Left(TransactionNotAllowedByScript(log, isAssetScript = false))
        case (_, Right(TRUE))       => Right(order)
        case (_, Right(x))          => Left(GenericError(s"Script returned not a boolean result, but $x"))
      }
    } match {
      case Failure(e) => Left(ScriptExecutionError(s"Uncaught execution error: $e", List.empty, isAssetScript = false))
      case Success(s) => s
    }

  def verifyExchange(et: ExchangeTransaction,
                     blockchain: Blockchain,
                     matcherScriptOpt: Option[Script],
                     height: Int): ValidationResult[Transaction] = {

    val typeId    = et.builder.typeId
    val sellOrder = et.sellOrder
    val buyOrder  = et.buyOrder

    def matcherTxVerification =
      matcherScriptOpt
        .map { script =>
          if (et.version != 1) {
            stats.accountScriptExecution
              .measureForType(typeId)(verifyTx(blockchain, script, height, et, None))
          } else {
            Left(GenericError("Can't process transaction with signature from scripted account"))
          }
        }
        .getOrElse(stats.signatureVerification.measureForType(typeId)(verifyAsEllipticCurveSignature(et)))

    def orderVerification(order: Order) = {
      blockchain
        .accountScript(order.sender.toAddress)
        .map { script =>
          if (order.version != 1) {
            stats.orderValidation.measure(verifyOrder(blockchain, script, height, order))
          } else {
            Left(GenericError("Can't process order with signature from scripted account"))
          }
        }
        .getOrElse(stats.signatureVerification.measureForType(typeId)(verifyAsEllipticCurveSignature(order)))
    }

    def assetVerification(assetId: Asset) = assetId match {
      case Waves => Right(et)
      case asset: IssuedAsset =>
        blockchain.assetScript(asset).fold[ValidationResult[Transaction]](Right(et)) { script =>
          verifyTx(blockchain, script, height, et, assetId.compatId) leftMap {
            case x: HasScriptType => x
            case GenericError(x)  => ScriptExecutionError(x, List.empty, isAssetScript = true)
            case x                => ScriptExecutionError(x.toString, List.empty, isAssetScript = true)
          }
        }
    }

    lazy val txAssetsVerification: ValidationResult[Transaction] = {
      Set(
        buyOrder.assetPair.amountAsset,
        buyOrder.assetPair.priceAsset,
        buyOrder.matcherFeeAssetId,
        sellOrder.matcherFeeAssetId
      ).foldLeft[ValidationResult[Transaction]](Right(et)) {
        case (verificationResult, asset) => verificationResult flatMap (_ => assetVerification(asset))
      }
    }

    for {
      _ <- matcherTxVerification
      _ <- orderVerification(sellOrder)
      _ <- orderVerification(buyOrder)
      _ <- txAssetsVerification
    } yield et
  }

  def verifyAsEllipticCurveSignature[T <: Proven with Authorized](pt: T): ValidationResult[T] =
    pt.proofs.proofs match {
      case p :: Nil =>
        Either.cond(crypto.verify(p.arr, pt.bodyBytes(), pt.sender),
                    pt,
                    GenericError(s"Script doesn't exist and proof doesn't validate as signature for $pt"))
      case _ => Left(GenericError("Transactions from non-scripted accounts must have exactly 1 proof"))
    }

  private def logged(id: => String, result: (Log, Either[String, EVALUATED])): (Log, Either[String, EVALUATED]) = {
    val (execLog, execResult) = result
    log.debug(s"Script for $id evaluated to $execResult")
    execLog.foreach {
      case (k, Right(v))  => log.debug(s"Evaluated `$k` to $v")
      case (k, Left(err)) => log.debug(s"Failed to evaluate `$k`: $err")
    }
    result
  }
}
