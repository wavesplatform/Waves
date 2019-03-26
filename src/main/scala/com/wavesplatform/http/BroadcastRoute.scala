package com.wavesplatform.http

import com.wavesplatform.api.http.{ApiError, ApiRoute}
import com.wavesplatform.network._
import com.wavesplatform.settings.{RestAPISettings, WavesSettings}
import com.wavesplatform.transaction.{Transaction, ValidationError}
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import monix.execution.Scheduler

import scala.concurrent.{ExecutionContextExecutor, Future}

private object BroadcastRoute {
  private[this] var executionContext = Option.empty[ExecutionContextExecutor]

  def executionContext(settings: RestAPISettings): ExecutionContextExecutor = {
    if (executionContext.isEmpty) {
      synchronized(if (executionContext.isEmpty) executionContext = Some(new ExecutionContextExecutor {
        private[this] val parallelism = settings.broadcastParallelism
        private[this] val scheduler = Scheduler.computation(parallelism, "rest-api-broadcast")

        override def reportFailure(cause: Throwable): Unit = scheduler.reportFailure(cause)
        override def execute(command: Runnable): Unit      = scheduler.execute(command)
      }))
    }

    executionContext.getOrElse(sys.error("Should not happen"))
  }
}

trait BroadcastRoute { self: ApiRoute =>
  def utx: UtxPool
  def allChannels: ChannelGroup

  protected lazy val broadcastExecutionContext = BroadcastRoute.executionContext(settings)

  protected def doBroadcast(v: Either[ValidationError, Transaction]): Future[Either[ApiError, Transaction]] =
    Future {
      val r = for {
        tx <- v
        r  <- utx.putIfNew(tx)
      } yield {
        val (added, _) = r
        if (added) allChannels.broadcastTx(tx, None)
        tx
      }

      r.left.map(ApiError.fromValidationError)
    }(broadcastExecutionContext)
}
