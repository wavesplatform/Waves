package scorex

import com.wavesplatform.UtxPool
import scorex.api.http.ApiError
import scorex.transaction.{Transaction, ValidationError}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait BroadcastRoute {
  def utx: UtxPool

  protected def doBroadcast(v: Either[ValidationError, Transaction]): Future[Either[ApiError, Transaction]] =
    Future(v.flatMap(t => utx.putIfNew(t)).left.map(ApiError.fromValidationError))
}
