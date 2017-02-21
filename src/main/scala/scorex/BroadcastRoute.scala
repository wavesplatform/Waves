package scorex

import scorex.api.http.{ApiError, StateCheckFailed}
import scorex.transaction.{Transaction, TransactionModule, ValidationError}

trait BroadcastRoute {
  def transactionModule: TransactionModule
  protected def doBroadcast[A <: Transaction](v: Either[ValidationError, A]) =
    v.left.map(ApiError.fromValidationError).flatMap(broadcast)

  protected def broadcast[T <: Transaction](tx: T): Either[ApiError, T] =
    if (transactionModule.onNewOffchainTransaction(tx)) Right(tx) else Left(StateCheckFailed)
}
