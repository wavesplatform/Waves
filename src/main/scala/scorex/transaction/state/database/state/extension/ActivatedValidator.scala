package scorex.transaction.state.database.state.extension

import scorex.settings.WavesHardForkParameters
import scorex.transaction.assets.exchange.ExchangeTransaction
import scorex.transaction.assets.{BurnTransaction, IssueTransaction, ReissueTransaction, TransferTransaction}
import scorex.transaction.{GenesisTransaction, PaymentTransaction, Transaction}

class ActivatedValidator(settings: WavesHardForkParameters) extends StateExtension {


  override def isValid(tx: Transaction, height: Int): Boolean = tx match {
    case tx: PaymentTransaction => true
    case gtx: GenesisTransaction => true
    case tx: TransferTransaction => true
    case tx: IssueTransaction => true
    case tx: ReissueTransaction => true
    case tx: BurnTransaction => tx.timestamp > settings.allowBurnTransactionAfterTimestamp
    case tx: ExchangeTransaction => true
    case _ => false
  }


  override def process(tx: Transaction, blockTs: Long, height: Int): Unit = {}
}