package com.wavesplatform.http

import com.wavesplatform.api.http.{ApiError, WithSettings}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.network._
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import monix.execution.Scheduler

import scala.concurrent.{ExecutionContext, Future}

trait BroadcastRoute { self: WithSettings =>
  def utxPoolSynchronizer: UtxPoolSynchronizer

  protected def broadcastIfSuccess(v: TracedResult[ValidationError, Transaction])(
      implicit ec: ExecutionContext = Scheduler.global): Future[TracedResult[ApiError, Transaction]] = {

    @inline
    def doBroadcastTx(transaction: Transaction)(implicit ec: ExecutionContext): Future[TracedResult[ValidationError, Transaction]] =
      utxPoolSynchronizer
        .publishTransaction(transaction, forceBroadcast = settings.allowTxRebroadcasting)
        .map(_.map(_ => transaction))

    @inline
    def doBroadcastEitherTx(v: TracedResult[ValidationError, Transaction])(
        implicit ec: ExecutionContext): Future[TracedResult[ValidationError, Transaction]] = v.resultE match {
      case Left(_)      => Future.successful(v)
      case Right(value) => doBroadcastTx(value).map(tr => tr.copy(trace = v.trace ::: tr.trace))
    }

    doBroadcastEitherTx(v).map(_.leftMap(ApiError.fromValidationError))
  }
}
