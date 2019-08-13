package com.wavesplatform.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.{ApiError, ApiRoute}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network._
import com.wavesplatform.transaction.Transaction
import play.api.libs.json.Reads

import scala.concurrent.Future

trait BroadcastRoute extends ApiRoute {
  def utxPoolSynchronizer: UtxPoolSynchronizer

  private def internalBroadcast[A: Reads](pathMatcher: String, f: A => Either[ValidationError, Transaction]): Route = extractExecutionContext { implicit ec =>
    json[A] { a =>
      f(a) match {
        case Left(error) => Future.successful[Either[ApiError, Transaction]](Left(ApiError.fromValidationError(error)))
        case Right(tx)   => utxPoolSynchronizer.publish(tx).map(_.left.map(ApiError.fromValidationError))
      }
    }
  }

  def broadcastWithAuth[A: Reads](pathMatcher: String, f: A => Either[ValidationError, Transaction]): Route = (path(pathMatcher) & post) {
    withAuth(internalBroadcast(pathMatcher, f))
  }

  def broadcast[A: Reads](pathMatcher: String, f: A => Either[ValidationError, Transaction]): Route = (path(pathMatcher) & post) {
    internalBroadcast(pathMatcher, f)
  }
}
