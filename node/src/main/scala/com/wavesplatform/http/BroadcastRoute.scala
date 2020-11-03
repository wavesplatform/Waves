package com.wavesplatform.http

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.{ApiError, ApiRoute, jsonPost}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network.UtxPoolSynchronizer
import com.wavesplatform.transaction.Transaction
import play.api.libs.json._

import scala.concurrent.Future

trait BroadcastRoute { _: ApiRoute =>
  def utxPoolSynchronizer: UtxPoolSynchronizer

  private def broadcastTransaction(tx: Transaction): Future[ToResponseMarshallable] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    utxPoolSynchronizer.processIncomingTransaction(tx, None).map(_.leftMap(ApiError.fromValidationError))
  }

  def broadcast[A: Reads](f: A => Either[ValidationError, Transaction]): Route =
    jsonPost[A] { a =>
      f(a).fold(
        ApiError.fromValidationError,
        broadcastTransaction
      )
    }
}
