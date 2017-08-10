package scorex

import com.wavesplatform.UtxPool
import io.netty.channel.group.ChannelGroup
import scorex.api.http.ApiError
import scorex.transaction.{Transaction, ValidationError}

import scala.concurrent.Future

trait BroadcastRoute {
  def utx: UtxPool
  def allChannels: ChannelGroup

  import scala.concurrent.ExecutionContext.Implicits.global

  protected def doBroadcast(v: Either[ValidationError, Transaction]): Future[Either[ApiError, Transaction]] = Future {
    v.flatMap(t => utx.putIfNew(t, UtxPool.broadcastTx(allChannels, None)))
      .left.map(ApiError.fromValidationError)
  }
}
