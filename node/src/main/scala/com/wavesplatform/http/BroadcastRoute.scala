package com.wavesplatform.http

import akka.http.scaladsl.marshalling.{ToResponseMarshallable, ToResponseMarshaller}
import akka.http.scaladsl.server.{Directive1, Route}
import com.wavesplatform.api.http.{ApiError, ApiRoute, jsonPostD}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.TransactionValidator
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import play.api.libs.json._

import scala.concurrent.Future

trait BroadcastRoute { _: ApiRoute =>
  def utxPoolSynchronizer: TransactionValidator

  private def broadcastTransaction(tx: Transaction, includeTrace: Boolean): Future[ToResponseMarshallable] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    implicit val trw: ToResponseMarshaller[TracedResult[ApiError, Transaction]] = tracedResultMarshaller(includeTrace)
    utxPoolSynchronizer.validateAndBroadcast(tx, None).map(_.leftMap(ApiError.fromValidationError).map(_ => tx))
  }

  private def extractTraceParameter(tx: Transaction): Directive1[ToResponseMarshallable] =
    parameter("trace".as[Boolean].?(false))
      .flatMap { includeTrace =>
        provide(broadcastTransaction(tx, includeTrace))
      }

  def broadcast[A: Reads](f: A => Either[ValidationError, Transaction]): Route =
    jsonPostD[A] { a =>
      f(a).fold(
        e => provide(ApiError.fromValidationError(e)),
        extractTraceParameter
      )
    }
}
