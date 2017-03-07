package scorex

import scorex.api.http.{ApiError, StateCheckFailed}
import scorex.transaction.{Transaction, TransactionModule, ValidationError}

trait BroadcastRoute {
  def transactionModule: TransactionModule

  protected def doBroadcast[A <: Transaction](v: Either[ValidationError, A]): Either[ApiError, A] =
    (for {
      tx <- v
      r <- transactionModule.onNewOffchainTransaction(tx)
    } yield r).left.map(ApiError.fromValidationError)
}
