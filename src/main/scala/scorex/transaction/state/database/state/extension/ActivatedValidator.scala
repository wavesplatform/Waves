package scorex.transaction.state.database.state.extension

import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import scorex.transaction.state.database.blockchain.StoredState
import scorex.transaction.{GenesisTransaction, PaymentTransaction, Transaction}

class ActivatedValidator(
                          allowBurnTransactionAfterTimestamp: Long,
                          allowLeaseTransactionAfterTimestamp: Long,
                          allowExchangeTransactionAfterTimestamp: Long
                        ) extends Validator {


  override def validate(storedState: StoredState, tx: Transaction, height: Int): Either[TransactionValidationError, Transaction] = tx match {
    case tx: BurnTransaction if tx.timestamp <= allowBurnTransactionAfterTimestamp =>
      Left(TransactionValidationError(tx, s"must not appear before time=$allowBurnTransactionAfterTimestamp"))
    case tx: LeaseTransaction if tx.timestamp <= allowLeaseTransactionAfterTimestamp =>
      Left(TransactionValidationError(tx, s"must not appear before time=$allowLeaseTransactionAfterTimestamp"))
    case tx: LeaseCancelTransaction if tx.timestamp <= allowLeaseTransactionAfterTimestamp =>
      Left(TransactionValidationError(tx, s"must not appear before time=$allowLeaseTransactionAfterTimestamp"))
    case tx: ExchangeTransaction if tx.timestamp <= allowExchangeTransactionAfterTimestamp =>
      Left(TransactionValidationError(tx, s"must not appear before time=$allowExchangeTransactionAfterTimestamp"))
    case _: BurnTransaction => Right(tx)
    case _: PaymentTransaction => Right(tx)
    case _: GenesisTransaction => Right(tx)
    case _: TransferTransaction => Right(tx)
    case _: IssueTransaction => Right(tx)
    case _: ReissueTransaction => Right(tx)
    case _: ExchangeTransaction => Right(tx)
    case _: LeaseTransaction => Right(tx)
    case _: LeaseCancelTransaction => Right(tx)
    case x => Left(TransactionValidationError(x, "Unknown transaction must be explicitly registered within ActivatedValidator"))
  }

  override def process(storedState: StoredState, tx: Transaction, blockTs: Long, height: Int): Unit = {}
}
