package scorex

import scorex.api.http.ApiError
import scorex.transaction.{NewTransactionHandler, Transaction, ValidationError}

trait BroadcastRoute {
  def transactionModule: NewTransactionHandler

  protected def doBroadcast[A <: Transaction](v: Either[ValidationError, A]): Either[ApiError, A] =
    (for {
      tx <- v
      r <- transactionModule.onNewOffchainTransaction(tx)
    } yield r).left.map(ApiError.fromValidationError)
}
