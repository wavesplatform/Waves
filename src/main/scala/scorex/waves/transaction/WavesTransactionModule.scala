package scorex.waves.transaction

import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http.NoBalance
import scorex.app.Application
import scorex.block.{Block, BlockField}
import scorex.crypto.encode.Base58
import scorex.settings.Settings
import scorex.transaction.SimpleTransactionModule.StoredInBlock
import scorex.transaction._
import scorex.utils.NTP

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * Waves Transaction Module
  */
class WavesTransactionModule(implicit override val settings: TransactionSettings with Settings, application: Application)
  extends SimpleTransactionModule() {

  val UnitsInWave = 100000000L
  val TotalWaves  = 100000000L
  override val InitialBalance = UnitsInWave * TotalWaves

  val GenesisTransactionsTimestamp = settings.genesisTimestamp


  /**
    * Publish signed payment transaction which generated outside node
    */
  def broadcastPayment(externalPayment: ExternalPayment): Try[PaymentTransaction] = {
    val time = externalPayment.timestamp
    val sigBytes = Base58.decode(externalPayment.signature).get
    val senderPubKey = Base58.decode(externalPayment.senderPublicKey).get
    val recipientAccount = new Account(externalPayment.recipient)
    val payment = new PaymentTransaction(new PublicKeyAccount(senderPubKey),
      recipientAccount, externalPayment.amount, externalPayment.fee, time, sigBytes)
    if (blockStorage.state.isValid(payment)) {
      onNewOffchainTransaction(payment)
      Success(payment)
    } else {
      Failure(new Exception(NoBalance.message))
    }
  }

  override def packUnconfirmed(): StoredInBlock = {
    clearIncorrectTransactions()

    val currentTime = NTP.correctedTime()

    blockStorage.state.validate(
      utxStorage.all()
        .filter(_.timestamp <= currentTime)
        .sortBy(-_.fee)
        .take(SimpleTransactionModule.MaxTransactionsPerBlock))
  }

  override def clearIncorrectTransactions(): Unit = {

    super.clearIncorrectTransactions()

    val currentTime = NTP.correctedTime()
    val txs = utxStorage.all()

    // Remove transactions which are too far in the future
    txs.filter {
      tx => (tx.timestamp - currentTime).millis > SimpleTransactionModule.MaxTimeForUnconfirmed
    } foreach {
      utxStorage.remove
    }
  }

  override def isValid(block: Block): Boolean = {
    val blockTimestamp = block.timestampField.value
    block.transactions.forall( _.timestamp <= blockTimestamp) && super.isValid(block)
  }

  override def genesisData: BlockField[SimpleTransactionModule.StoredInBlock] = {


    val totalBalance = InitialBalance
    val txs = List(
      GenesisTransaction( new Account("3N5jhcA7R98AUN12ee9pB7unvnAKfzb3nen"), totalBalance - 5 * UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("3MyTvqfeLWkvjSZ1hwkhQjzipZr7Pk8dyMR"), UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("3MqS3mVY4Yr4HoTdpWiEaq9phwbaoWS2W6A"), UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("3N3CDuzGXB2qP5vb2NvnnDQ68HahNCfYVBg"), UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("3N2sacZ9XTQUkLDdZZgtb1zJUAmr6oziRrU"), UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("3N189PMB8BaxngN3fNvDRkFbvbH8xMkk328"), UnitsInWave, GenesisTransactionsTimestamp)
    )
    require(txs.foldLeft(0L)(_ + _.amount) == InitialBalance)


    TransactionsBlockField(txs)
  }
}
