package com.wavesplatform.network

import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Transaction
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.smart.script.trace.TracedResult
import com.wavesplatform.utils.Schedulers.ExecutorExt
import com.wavesplatform.utils.ScorexLogging
import com.wavesplatform.utx.UtxPool
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import monix.execution.Scheduler

import scala.concurrent.{ExecutionException, Future}
import scala.util.Success

class UtxPoolSynchronizer(utx: UtxPool, allChannels: ChannelGroup, timedScheduler: Scheduler) extends ScorexLogging {

  import Scheduler.Implicits.global

  def processIncomingTransaction(
      tx: Transaction,
      allowRebroadcast: Boolean,
      source: Option[Channel]
  ): Future[TracedResult[ValidationError, Boolean]] =
    timedScheduler
      .executeCatchingInterruptedException(utx.putIfNew(tx, source.isEmpty))
      .recover {
        case err: ExecutionException if err.getCause.isInstanceOf[InterruptedException] =>
          log.trace(s"Transaction took too long to validate: ${tx.id()}")
          TracedResult(Left(GenericError("Transaction took too long to validate")))
        case err =>
          log.warn(s"Error validating transaction ${tx.id()}", err)
          TracedResult(Left(GenericError(err)))
      }
      .andThen {
        case Success(TracedResult(Right(isNew), _)) if isNew || allowRebroadcast => allChannels.broadcast(tx, source)
      }
}
