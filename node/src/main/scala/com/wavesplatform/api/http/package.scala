package com.wavesplatform.api

import java.util.NoSuchElementException
import java.util.concurrent.ExecutionException

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import com.wavesplatform.account.{Address, PublicKey}
import com.wavesplatform.api.http.ApiError.{InvalidAssetId, InvalidBlockId, InvalidPublicKey, InvalidSignature, InvalidTransactionId, WrongJson}
import com.wavesplatform.api.http.requests.DataRequest._
import com.wavesplatform.api.http.requests.SponsorFeeRequest._
import com.wavesplatform.api.http.requests._
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.crypto
import com.wavesplatform.http.{ApiMarshallers, PlayJsonException}
import com.wavesplatform.lang.contract.meta.RecKeyValueFolder
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.assets._
import com.wavesplatform.transaction.assets.exchange.ExchangeTransaction
import com.wavesplatform.transaction.lease._
import com.wavesplatform.transaction.smart.{InvokeScriptTransaction, SetScriptTransaction}
import com.wavesplatform.transaction.transfer._
import com.wavesplatform.utils.ScorexLogging
import monix.execution.Scheduler
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

package object http extends ApiMarshallers with ScorexLogging {
  val versionReads: Reads[Byte] = {
    val defaultByteReads = implicitly[Reads[Byte]]
    val intToByteReads   = implicitly[Reads[Int]].map(_.toByte)
    val stringToByteReads = implicitly[Reads[String]]
      .map(s => Try(s.toByte))
      .collect(JsonValidationError("Can't parse version")) {
        case Success(v) => v
      }

    defaultByteReads orElse
      intToByteReads orElse
      stringToByteReads
  }

  def createTransaction(senderPk: String, jsv: JsObject)(txToResponse: Transaction => ToResponseMarshallable): ToResponseMarshallable = {
    val typeId = (jsv \ "type").as[Byte]

    (jsv \ "version").validateOpt[Byte](versionReads) match {
      case JsError(errors) => WrongJson(None, errors)
      case JsSuccess(value, _) =>
        val version = value.getOrElse(1: Byte)
        val txJson  = jsv ++ Json.obj("version" -> version)

        PublicKey
          .fromBase58String(senderPk)
          .flatMap { senderPk =>
            TransactionParsers.by(typeId, version) match {
              case None => Left(GenericError(s"Bad transaction type ($typeId) and version ($version)"))
              case Some(x) =>
                x match {
                  case TransferTransaction       => txJson.as[TransferRequest].toTxFrom(senderPk)
                  case CreateAliasTransaction    => txJson.as[CreateAliasRequest].toTxFrom(senderPk)
                  case LeaseTransaction          => txJson.as[LeaseRequest].toTxFrom(senderPk)
                  case LeaseCancelTransaction    => txJson.as[LeaseCancelRequest].toTxFrom(senderPk)
                  case ExchangeTransaction       => txJson.as[ExchangeRequest].toTxFrom(senderPk)
                  case IssueTransaction          => txJson.as[IssueRequest].toTxFrom(senderPk)
                  case ReissueTransaction        => txJson.as[ReissueRequest].toTxFrom(senderPk)
                  case BurnTransaction           => txJson.as[BurnRequest].toTxFrom(senderPk)
                  case MassTransferTransaction   => TransactionFactory.massTransferAsset(txJson.as[MassTransferRequest], senderPk)
                  case DataTransaction           => TransactionFactory.data(txJson.as[DataRequest], senderPk)
                  case InvokeScriptTransaction   => TransactionFactory.invokeScript(txJson.as[InvokeScriptRequest], senderPk)
                  case SetScriptTransaction      => TransactionFactory.setScript(txJson.as[SetScriptRequest], senderPk)
                  case SetAssetScriptTransaction => TransactionFactory.setAssetScript(txJson.as[SetAssetScriptRequest], senderPk)
                  case SponsorFeeTransaction     => TransactionFactory.sponsor(txJson.as[SponsorFeeRequest], senderPk)
                }
            }
          }
          .fold(ApiError.fromValidationError, txToResponse)
    }
  }

  def parseOrCreateTransaction(jsv: JsObject)(txToResponse: Transaction => ToResponseMarshallable): ToResponseMarshallable = {
    val result = TransactionFactory.fromSignedRequest(jsv)
    if (result.isRight) {
      result.fold(ApiError.fromValidationError, txToResponse)
    } else {
      createTransaction((jsv \ "senderPk").as[String], jsv)(txToResponse)
    }
  }

  private def base58Segment(requiredLength: Option[Int], error: String => ApiError): PathMatcher1[ByteStr] = Segment.map { str =>
    ByteStr.decodeBase58(str) match {
      case Success(value) if requiredLength.forall(_ == value.arr.length) => value
      case _                                                          => throw ApiException(error(str))
    }
  }

  private def idOrHash(error: String => ApiError): PathMatcher1[ByteStr] = Segment.map { str =>
    ByteStr.decodeBase58(str) match {
      case Success(value) =>
        if (value.arr.length == crypto.DigestLength || value.arr.length == crypto.SignatureLength) value
        else throw ApiException(error(s"$str has invalid length ${value.arr.length}. Length can either be ${crypto.DigestLength} or ${crypto.SignatureLength}"))
      case Failure(exception) =>
        throw ApiException(error(exception.getMessage))
    }
  }

  val TransactionId: PathMatcher1[ByteStr] = idOrHash(InvalidTransactionId)
  val BlockId: PathMatcher1[ByteStr] = idOrHash(InvalidBlockId)

  val AssetId: PathMatcher1[IssuedAsset] = base58Segment(Some(crypto.DigestLength), _ => InvalidAssetId).map(IssuedAsset)

  val Signature: PathMatcher1[ByteStr] = base58Segment(Some(crypto.SignatureLength), _ => InvalidSignature)

  val AddrSegment: PathMatcher1[Address] = Segment.map { str =>
    (for {
      bytes <- Try(Base58.decode(str)).fold(e => Left(GenericError(e)), Right(_))
      addr  <- Address.fromBytes(bytes)
    } yield addr).fold(ae => throw ApiException(ApiError.fromValidationError(ae)), identity)
  }

  val PublicKeySegment: PathMatcher1[PublicKey] = base58Segment(Some(crypto.KeyLength), _ => InvalidPublicKey).map(s => PublicKey(s))

  private val jsonRejectionHandler = RejectionHandler
    .newBuilder()
    .handle { case ValidationRejection(_, Some(PlayJsonException(cause, errors))) => complete(WrongJson(cause, errors)) }
    .result()

  private val jsonExceptionHandler: ExceptionHandler = ExceptionHandler {
    case JsResultException(err)                                         => complete(WrongJson(errors = err))
    case PlayJsonException(cause, errors)                               => complete(WrongJson(cause, errors))
    case e: NoSuchElementException                                      => complete(WrongJson(Some(e)))
    case e: IllegalArgumentException                                    => complete(ApiError.fromValidationError(GenericError(e)))
    case e: AssertionError                                              => complete(ApiError.fromValidationError(GenericError(e)))
    case e: ExecutionException if e.getCause != null && e.getCause != e => jsonExceptionHandler(e.getCause)
  }

  def jsonPost[A: Reads](f: A => ToResponseMarshallable): Route =
    post((handleExceptions(jsonExceptionHandler) & handleRejections(jsonRejectionHandler)) {
      entity(as[A]) { a =>
        complete(f(a))
      }
    }) ~ get(complete(StatusCodes.MethodNotAllowed))

  def jsonParammedPost[A: Reads](f: (A, Map[String, String]) => ToResponseMarshallable): Route =
    post((handleExceptions(jsonExceptionHandler) & handleRejections(jsonRejectionHandler)) {
      parameterMap { params =>
        entity(as[A]) { a =>
          complete(f(a, params))
        }
      }
    }) ~ get(complete(StatusCodes.MethodNotAllowed))

  def extractScheduler: Directive1[Scheduler] = extractExecutionContext.map(ec => Scheduler(ec))

  val uncaughtExceptionHandler: ExceptionHandler = ExceptionHandler {
    case ApiException(error)   => complete(error)
    case e: StackOverflowError => log.error("Stack overflow error", e); complete(ApiError.Unknown)
    case NonFatal(e)           => log.error("Uncaught error", e); complete(ApiError.Unknown)
  }

  /** Handles all [[scala.util.control.NonFatal non-fatal]] exceptions and tries to handle fatal errors.
    *
    * This directive can't handle __fatal__ errors from:
    *
    *   - Monix [[monix.eval.Task tasks]] with async boundaries:
    *     {{{
    *       get(complete(Task(throw new StackOverflowError()).executeAsync.runToFuture))
    *       get(complete(Task.evalAsync(throw new StackOverflowError()).runToFuture))
    *       get(complete(Task.deferFuture(Future(throw new StackOverflowError())).runToFuture))
    *     }}}
    *   - Async futures (i.e. which are not available at the time of handling):
    *     {{{
    *       get(complete(Future(throw new StackOverflowException())))
    *     }}}
    */
  def handleAllExceptions: Directive0 =
    Directive { inner => ctx =>
      val handleExceptions = uncaughtExceptionHandler.andThen(_(ctx))
      try inner(())(ctx).recoverWith(handleExceptions)(ctx.executionContext)
      catch {
        case thr: Throwable => uncaughtExceptionHandler.andThen(_(ctx)).applyOrElse[Throwable, Future[RouteResult]](thr, throw _)
      }
    }

  lazy val metaConverter: RecKeyValueFolder[JsValueWrapper, JsObject] =
    RecKeyValueFolder(
      Json.toJsFieldJsValueWrapper(_),
      l => Json.arr(l: _*),
      m => Json.obj(m: _*)
    )
}
