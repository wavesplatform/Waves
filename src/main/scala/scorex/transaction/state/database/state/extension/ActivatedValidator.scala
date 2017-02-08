package scorex.transaction.state.database.state.extension

import scorex.transaction.ValidationError.StateValidationError
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.{GenesisTransaction, PaymentTransaction, Transaction}

class ActivatedValidator(allowBurnTransactionAfterTimestamp: Long) extends Validator {


  override def isValid(tx: Transaction, height: Int): Either[StateValidationError, Transaction] = tx match {
    case tx: BurnTransaction if tx.timestamp <= allowBurnTransactionAfterTimestamp =>
      Left(StateValidationError(s"BurnTranaction(time: ${tx.timestamp}) must not appear before time=$allowBurnTransactionAfterTimestamp"))
    case _: PaymentTransaction => Right(tx)
    case gtx: GenesisTransaction => Right(tx)
    case tx: TransferTransaction => Right(tx)
    case tx: IssueTransaction => Right(tx)
    case tx: ReissueTransaction => Right(tx)
    case tx: ExchangeTransaction => Right(tx)
    case x => Left(StateValidationError(s"Unknown transaction $x must be explicitly registered within ActivatedValidator"))
  }

  override def process(tx: Transaction, blockTs: Long, height: Int): Unit = {}
}