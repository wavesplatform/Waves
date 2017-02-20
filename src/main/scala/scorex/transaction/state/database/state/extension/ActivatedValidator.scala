package scorex.transaction.state.database.state.extension

import scorex.transaction.ValidationError.StateValidationError
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.{GenesisTransaction, PaymentTransaction, Transaction}

class ActivatedValidator(allowBurnTransactionAfterTimestamp: Long) extends Validator {


  override def validate(tx: Transaction, height: Int): Either[StateValidationError, Transaction] = tx match {
    case tx: BurnTransaction if tx.timestamp <= allowBurnTransactionAfterTimestamp =>
      Left(StateValidationError(s"BurnTranaction(time: ${tx.timestamp}) must not appear before time=$allowBurnTransactionAfterTimestamp"))
    case _: BurnTransaction => Right(tx)
    case _: PaymentTransaction => Right(tx)
    case _: GenesisTransaction => Right(tx)
    case _: TransferTransaction => Right(tx)
    case _: IssueTransaction => Right(tx)
    case _: ReissueTransaction => Right(tx)
    case _: ExchangeTransaction => Right(tx)
    case x => Left(StateValidationError(s"Unknown transaction $x must be explicitly registered within ActivatedValidator"))
  }

  override def process(tx: Transaction, blockTs: Long, height: Int): Unit = {}
}