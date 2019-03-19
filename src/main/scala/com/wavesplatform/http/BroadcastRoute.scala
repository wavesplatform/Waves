package com.wavesplatform.http

import com.wavesplatform.api.http.ApiError
import com.wavesplatform.network._
import com.wavesplatform.transaction.{Transaction, ValidationError}
import com.wavesplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import monix.execution.Scheduler

import scala.concurrent.{ExecutionContextExecutor, Future}

private[api] object BroadcastRoute {
  val executionContext = new ExecutionContextExecutor {
    //noinspection ScalaStyle
    private[this] val scheduler = Scheduler.computation(4, "rest-api-broadcast")

    override def reportFailure(cause: Throwable): Unit = scheduler.reportFailure(cause)
    override def execute(command: Runnable): Unit = scheduler.execute(command)
  }
}

trait BroadcastRoute {
  def utx: UtxPool
  def allChannels: ChannelGroup

  protected def doBroadcast(v: Either[ValidationError, Transaction]): Future[Either[ApiError, Transaction]] = Future {
    val r = for {
      tx <- v
      r  <- utx.putIfNew(tx)
    } yield {
      val (added, _) = r
      if (added) allChannels.broadcastTx(tx, None)
      tx
    }

    r.left.map(ApiError.fromValidationError)
  }(BroadcastRoute.executionContext)
}
