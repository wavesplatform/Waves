package scorex.waves.transaction

import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.app.Application
import scorex.block.BlockField
import scorex.crypto.encode.Base58
import scorex.settings.Settings
import scorex.transaction._
import scorex.utils.NTP

/**
  * Waves Transaction Module
  */
class WavesTransactionModule(implicit override val settings: TransactionSettings with Settings, application: Application)
  extends SimpleTransactionModule() {

  /**
    * Publish signed payment transaction which generated outside node
    */
  def broadcastPayment(externalPayment: ExternalPayment): PaymentTransaction = {
    val time = externalPayment.timestamp
    val sigBytes = Base58.decode(externalPayment.signature).get
    val senderPubKey = Base58.decode(externalPayment.senderPublicKey).get
    val recipientAccount = new Account(externalPayment.recipient)
    val payment = new PaymentTransaction(new PublicKeyAccount(senderPubKey),
      recipientAccount, externalPayment.amount, externalPayment.fee, time, sigBytes)
    if (blockStorage.state.isValid(payment))
      onNewOffchainTransaction(payment)
    payment
  }

  override def genesisData: BlockField[SimpleTransactionModule.StoredInBlock] = {
    val ipoMembers = List(
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
