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

  val UnitsInWave = 100000000L
  val TotalWaves  = 100000000L
  override val InitialBalance = UnitsInWave * TotalWaves

  val GenesisTransactionsTimestamp = 0L


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
//    val ipoMembers = List(
//      "2nDESCmSiTbcuutek3nJHGKevzgkycxFH9Y"
//    )

    val totalBalance = InitialBalance
    val txs = List(
      GenesisTransaction( new Account("2nDESCmSiTbcuutek3nJHGKevzgkycxFH9Y"), totalBalance - 5 * UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("2nGSYaXexawHYz4wnCBMLzeEtbCVZQSvKRr"), UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("2nQRK9oxzYwg364Cur3N4uqeeFB9U7VDGcV"), UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("2nJHZ17iWSjMsZgCTCxUPoGzeX7hArqoyky"), UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("2n6CbQ2cufQ2ZkmMU3TzGGYa2AGKveqJjSd"), UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("2n7LeuEiZ4Te1zphx3gqGziuNJQ4gL4f2kt"), UnitsInWave, GenesisTransactionsTimestamp)
    )
    require(txs.foldLeft(0L)(_ + _.amount) == InitialBalance)

//    val txs: List[GenesisTransaction] = ipoMembers.map { address =>
//      val recipient = new Account(address)
//      GenesisTransaction(recipient, totalBalance / ipoMembers.length, GenesisTransactionsTimestamp)
//    }

    TransactionsBlockField(txs)
  }
}
