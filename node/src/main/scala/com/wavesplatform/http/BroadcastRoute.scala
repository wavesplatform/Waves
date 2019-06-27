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

  protected def doBroadcasTx(transaction: Transaction)(
    implicit ec: ExecutionContext = utxPoolSynchronizer.scheduler): Future[TracedResult[ValidationError, Transaction]] =
    utxPoolSynchronizer
      .publishTransaction(transaction, forceBroadcast = settings.allowTxRebroadcasting)
      .map(_.map(_ => transaction))

  protected def doBroadcastEitherTx(v: TracedResult[ValidationError, Transaction])(
    implicit ec: ExecutionContext = utxPoolSynchronizer.scheduler): Future[TracedResult[ValidationError, Transaction]] = v.resultE match {
    case Left(_) => Future.successful(v)
    case Right(value) => doBroadcasTx(value).map(tr => tr.copy(trace = v.trace ::: tr.trace))
  }

  protected def doBroadcast(v: TracedResult[ValidationError, Transaction])(
    implicit ec: ExecutionContext = utxPoolSynchronizer.scheduler): Future[TracedResult[ApiError, Transaction]] =
    doBroadcastEitherTx(v).map(_.leftMap(ApiError.fromValidationError))
}
