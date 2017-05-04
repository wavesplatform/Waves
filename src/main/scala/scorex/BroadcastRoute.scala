package scorex

import scorex.api.http.{ApiError, StateCheckFailed}
import scorex.transaction.{Transaction, NewTransactionHandler, ValidationError}

trait BroadcastRoute {
  def transactionModule: NewTransactionHandler

  protected def doBroadcast[A <: Transaction](v: Either[ValidationError, A]): Either[ApiError, A] =
    (for {
      tx <- v
      r <- transactionModule.onNewOffchainTransaction(tx)
    } yield r).left.map(ApiError.fromValidationError)
}
