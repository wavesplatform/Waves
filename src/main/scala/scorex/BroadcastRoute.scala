package scorex

import com.wavesplatform.UtxPool
import com.wavesplatform.network._
import io.netty.channel.group.ChannelGroup
import play.api.libs.json.JsObject
import scorex.api.http.ApiError
import scorex.api.http.assets.SignedTransferRequest
import scorex.transaction.{Transaction, ValidationError}

import scala.concurrent.Future

trait BroadcastRoute {
  def utx: UtxPool

  def allChannels: ChannelGroup

  import scala.concurrent.ExecutionContext.Implicits.global

  protected def doBroadcast(v: Either[ValidationError, Transaction]): Future[Either[ApiError, Transaction]] = Future {
    (for {
      tx <- v
      utxResult <- utx.putIfNew(tx)
    } yield {
      if (utxResult) {
        allChannels.broadcastTx(tx, None)
      }
      tx
    }).left.map(ApiError.fromValidationError)
  }

  protected def addToUtx(req: SignedTransferRequest): Either[ValidationError, (Transaction, Boolean)] = for {
    tx <- req.toTx
    added <- utx.putIfNew(tx)
  } yield (tx, added)

  protected def toResponse(x: Either[ValidationError, Transaction]): JsObject = x match {
    case Left(e) => ApiError.fromValidationError(e).json
    case Right(tx) => tx.json()
  }
}
