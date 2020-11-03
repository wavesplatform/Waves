package com.wavesplatform.network

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utils.Schedulers.ExecutorExt
import com.wavesplatform.utils.ScorexLogging
import io.netty.channel.Channel
import monix.execution.Scheduler

import scala.concurrent.{ExecutionException, Future}
import scala.util.Success

trait UtxPoolSynchronizer {
  def processIncomingTransaction(tx: Transaction, source: Option[Channel]): Future[TracedResult[ValidationError, Boolean]]
}

object UtxPoolSynchronizer extends ScorexLogging {

  import Scheduler.Implicits.global

  def apply(
      putIfNew: (Transaction, Boolean) => TracedResult[ValidationError, Boolean],
      broadcast: (Transaction, Option[Channel]) => Unit,
      timedScheduler: Scheduler,
      allowRebroadcast: Boolean
  ): UtxPoolSynchronizer = { (tx, source) =>
    timedScheduler
      .executeCatchingInterruptedException(putIfNew(tx, source.isEmpty))
      .recover {
        case err: ExecutionException if err.getCause.isInstanceOf[InterruptedException] =>
          log.trace(s"Transaction took too long to validate: ${tx.id()}")
          TracedResult(Left(GenericError("Transaction took too long to validate")))
        case err =>
          log.warn(s"Error validating transaction ${tx.id()}", err)
          TracedResult(Left(GenericError(err)))
      }
      .andThen {
        case Success(TracedResult(Right(isNew), _)) if isNew || (allowRebroadcast && source.isEmpty) => broadcast(tx, source)
      }
  }
}
