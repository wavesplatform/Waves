package scorex

import com.wavesplatform.UtxPool
import com.wavesplatform.network.{RawBytes, TransactionMessageSpec}
import io.netty.channel.group.ChannelGroup
import scorex.api.http.ApiError
import scorex.transaction.{Transaction, ValidationError}
import com.wavesplatform.network._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait BroadcastRoute {
  def utx: UtxPool
  def allChannels: ChannelGroup

  protected def doBroadcast(v: Either[ValidationError, Transaction]): Future[Either[ApiError, Transaction]] =
    Future(v.flatMap(t => utx.putIfNew(t)
      .map(t => {
        allChannels.broadcast(RawBytes(TransactionMessageSpec.messageCode, t.bytes))
        t
      })).left.map(ApiError.fromValidationError))
}
