package scorex

import com.wavesplatform.network.OffChainTransaction
import io.netty.channel.Channel
import scorex.api.http.ApiError
import scorex.transaction.{NewTransactionHandler, Transaction, ValidationError}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

trait BroadcastRoute {
  def localChannel: Channel

  protected def doBroadcast(v: Either[ValidationError, Transaction]): Future[Either[ApiError, Transaction]] =
    v match {
      case Right(t) =>
        val p = Promise[Either[ValidationError, Transaction]]
        localChannel.writeAndFlush(OffChainTransaction(t, p))
        p.future.map(_.left.map(ApiError.fromValidationError))
      case Left(e) =>
        Future.successful(Left(ApiError.fromValidationError(e)))
    }
}
