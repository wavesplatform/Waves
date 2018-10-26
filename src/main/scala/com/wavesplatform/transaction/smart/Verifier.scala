package com.wavesplatform.transaction.smart

import cats.implicits._
import com.google.common.base.Throwables
import com.wavesplatform.crypto
import com.wavesplatform.lang.v1.compiler.Terms.{CONST_BOOLEAN, EVALUATED}
import com.wavesplatform.matcher.smart.MatcherScriptRunner
import com.wavesplatform.metrics._
import com.wavesplatform.state._
import com.wavesplatform.transaction.ValidationError.{GenericError, ScriptExecutionError, TransactionNotAllowedByScript}
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.wavesplatform.transaction.smart.script.{Script, ScriptRunner}
import com.wavesplatform.utils.ScorexLogging
import kamon.Kamon
import shapeless.{:+:, CNil, Coproduct}

import scala.util.{Failure, Success, Try}

object Verifier extends Instrumented with ScorexLogging {

  val stats = TxProcessingStats

  import stats.TxTimerExt

  val orderScriptExecutionTime = Kamon.histogram("order-script-execution-time")
  val orderscriptsExecuted     = Kamon.counter("order-scripts-executed")

  val orderSignatureVerificationTime = Kamon.histogram("order-signature-verification-time")
  val orderSignaturesVerified        = Kamon.counter("order-signatures-verified")

  private type TxOrd = Transaction :+: Order :+: CNil

  def apply(blockchain: Blockchain, currentBlockHeight: Int)(tx: Transaction): Either[ValidationError, Transaction] =
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
              .measureForType(pt.builder.typeId)(verifyTx(blockchain, script, currentBlockHeight, pt, false))
          case _ =>
            stats.signatureVerification
              .measureForType(tx.builder.typeId)(verifyAsEllipticCurveSignature(pt))
        }
    }).flatMap(
      tx =>
        tx.checkedAssets
          .flatMap(assetId => blockchain.assetDescription(assetId).flatMap(_.script))
          .foldRight(Either.right[ValidationError, Transaction](tx)) { (script, txr) =>
            txr.right.flatMap(tx =>
              stats.assetScriptExecution
                .measureForType(tx.builder.typeId)(verifyTx(blockchain, script, currentBlockHeight, tx, true)))
        })

  def verifyTx(blockchain: Blockchain,
               script: Script,
               height: Int,
               transaction: Transaction,
               isTokenScript: Boolean): Either[ValidationError, Transaction] =
    Try {
      ScriptRunner[EVALUATED](height, Coproduct[TxOrd](transaction), blockchain, script) match {
        case (log, Left(execError)) => Left(ScriptExecutionError(execError, script.text, log, isTokenScript))
        case (log, Right(CONST_BOOLEAN(false))) =>
          Left(TransactionNotAllowedByScript(log, script.text, isTokenScript))
        case (_, Right(CONST_BOOLEAN(true))) => Right(transaction)
        case (_, Right(x))                   => Left(GenericError(s"Script returned not a boolean result, but $x"))
      }
    } match {
      case Failure(e) =>
        Left(
          ScriptExecutionError(
            s"""
      |Uncaught execution error.
      |Probably script does not return boolean, and can't validate any transaction or order: ${Throwables.getStackTraceAsString(e)}
    """.stripMargin,
            script.text,
            List.empty,
            isTokenScript
          ))
      case Success(s) => s
    }

  def verifyOrder(blockchain: Blockchain, script: Script, height: Int, order: Order): Either[ValidationError, Order] =
    Try {
      MatcherScriptRunner[EVALUATED](script, order) match {
        case (ctx, Left(execError))             => Left(ScriptExecutionError(execError, script.text, ctx, isTokenScript = false))
        case (ctx, Right(CONST_BOOLEAN(false))) => Left(TransactionNotAllowedByScript(ctx, script.text, isTokenScript = false))
        case (_, Right(CONST_BOOLEAN(true)))    => Right(order)
        case (_, Right(x))                      => Left(GenericError(s"Script returned not a boolean result, but $x"))
      }
    } match {
      case Failure(e) =>
        Left(
          ScriptExecutionError(
            s"""
      |Uncaught execution error.
      |Probably script does not return Boolean, and can't validate any transaction or order: $e
    """.stripMargin,
            script.text,
            List.empty,
            false
          ))
      case Success(s) => s
    }

  def verifyExchange(et: ExchangeTransaction,
                     blockchain: Blockchain,
                     matcherScriptOpt: Option[Script],
                     height: Int): Either[ValidationError, Transaction] = {
    val typeId    = et.builder.typeId
    val sellOrder = et.sellOrder
    val buyOrder  = et.buyOrder

    def matcherTxVerification =
      matcherScriptOpt
        .map { script =>
          if (et.version != 1) {
            stats.accountScriptExecution
              .measureForType(typeId)(verifyTx(blockchain, script, height, et, false))
          } else {
            Left(GenericError("Can't process transaction with signature from scripted account"))
          }
        }
        .getOrElse(stats.signatureVerification.measureForType(typeId)(verifyAsEllipticCurveSignature(et)))

    def sellerOrderVerification =
      blockchain
        .accountScript(sellOrder.sender.toAddress)
        .map(script =>
          if (sellOrder.version != 1) {
            stats.orderValidation.measure(verifyOrder(blockchain, script, height, sellOrder))
          } else {
            Left(GenericError("Can't process order with signature from scripted account"))
        })
        .getOrElse(stats.signatureVerification.measureForType(typeId)(verifyAsEllipticCurveSignature(sellOrder)))

    def buyerOrderVerification =
      blockchain
        .accountScript(buyOrder.sender.toAddress)
        .map(script =>
          if (buyOrder.version != 1) {
            stats.orderValidation.measure(verifyOrder(blockchain, script, height, buyOrder))
          } else {
            Left(GenericError("Can't process order with signature from scripted account"))
        })
        .getOrElse(stats.signatureVerification.measureForType(typeId)(verifyAsEllipticCurveSignature(buyOrder)))

    for {
      _ <- matcherTxVerification
      _ <- sellerOrderVerification
      _ <- buyerOrderVerification
    } yield et
  }

  def verifyAsEllipticCurveSignature[T <: Proven with Authorized](pt: T): Either[ValidationError, T] =
    pt.proofs.proofs match {
      case p :: Nil =>
        Either.cond(crypto.verify(p.arr, pt.bodyBytes(), pt.sender.publicKey),
                    pt,
                    GenericError(s"Script doesn't exist and proof doesn't validate as signature for $pt"))
      case _ => Left(GenericError("Transactions from non-scripted accounts must have exactly 1 proof"))
    }
}
