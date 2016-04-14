package scorex.waves.transaction

import scorex.account.Account
import scorex.app.Application
import scorex.block.BlockField
import scorex.settings.Settings
import scorex.transaction.{GenesisTransaction, SimpleTransactionModule, TransactionSettings, TransactionsBlockField}

/**
  *
  */
class WavesTransactionModule(implicit override val settings: TransactionSettings with Settings, application: Application)
  extends SimpleTransactionModule() {

  override def genesisData: BlockField[SimpleTransactionModule.StoredInBlock] = {
    val ipoMembers = List(
      //peer 1 accounts
      "jACSbUoHi4eWgNu6vzAnEx583NwmUAVfS",
      "kVVAu6F21Ax2Ugddms4p5uXz4kdZfAp8g",
      "bGbB5M5h9NBg2UM6KschsMky1SGm2Gdum"
    )

    val timestamp = 0L
    val totalBalance = InitialBalance

    val txs = ipoMembers.map { address =>
      val recipient = new Account(address)
      GenesisTransaction(recipient, totalBalance / ipoMembers.length, timestamp)
    }

    TransactionsBlockField(txs)
  }
}
