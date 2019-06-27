package com.wavesplatform.http

import com.wavesplatform.api.http.{ApiError, WithSettings}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network._
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult

import scala.concurrent.{ExecutionContext, Future}

trait BroadcastRoute {
  self: WithSettings =>
  def utxPoolSynchronizer: UtxPoolSynchronizer

  protected def doBroadcastVE(transaction: Transaction)(implicit ec: ExecutionContext): Future[TracedResult[ValidationError, Transaction]] =
    utxPoolSynchronizer
      .publishTransaction(transaction, forceBroadcast = settings.allowTxRebroadcasting)
      .map(_.map(_ => transaction))

  protected def doBroadcast(v: TracedResult[ValidationError, Transaction])(implicit ec: ExecutionContext): Future[TracedResult[ApiError, Transaction]] = v.resultE match {
    case Left(_) => Future.successful(v.leftMap(ApiError.fromValidationError))
    case Right(value) => doBroadcastVE(value).map(_.leftMap(ApiError.fromValidationError))
  }
}
