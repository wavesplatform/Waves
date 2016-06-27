package scorex.waves.transaction

import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.app.Application
import scorex.block.BlockField
import scorex.crypto.encode.Base58
import scorex.settings.Settings
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.transaction.LagonakiTransaction.ValidationResult.ValidationResult
import scorex.transaction._
import scorex.transaction.state.wallet.Payment
import scorex.utils.NTP
import scorex.wallet.Wallet
import scorex.waves.settings.WavesSettings

/**
  * Waves Transaction Module
  */
class WavesTransactionModule(implicit override val settings: TransactionSettings with Settings, application: Application)
  extends SimpleTransactionModule() {

  val UnitsInWave = 100000000L
  val TotalWaves  = 100000000L
  override val InitialBalance = UnitsInWave * TotalWaves

  val GenesisTransactionsTimestamp = settings.genesisTimestamp
  // TODO: remove asInstanceOf after Scorex update
  val minimumTxFee = settings.asInstanceOf[WavesSettings].minimumTxFee

  /**
    * Sign payment by keys from wallet
    *
    * TODO: Should be moved to Scorex
    */
  def signPayment(payment: Payment, wallet: Wallet): Option[PaymentTransaction] = {
    wallet.privateKeyAccount(payment.sender).map { sender =>
      signPayment(sender, new Account(payment.recipient), payment.amount, payment.fee, NTP.correctedTime())
    }
  }

  /**
    * Create signed PaymentTransaction without broadcasting to network
    *
    * TODO: Should be moved to Scorex
    */
  def signPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long, timestamp: Long): PaymentTransaction = {
    val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, timestamp, Array.empty)
    val payment = new PaymentTransaction(sender, recipient, amount, fee, timestamp, Array.empty, sig)
    payment
  }

  /**
    * Create signed payment transaction and validate it through current state.
    */
  def createSignedPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long, timestamp: Long): Either[PaymentTransaction, ValidationResult] = {
    val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, timestamp, Array.empty)
    val payment = new PaymentTransaction(sender, recipient, amount, fee, timestamp, Array.empty, sig)

    payment.validate match {
      case ValidationResult.ValidateOke => {
        if (blockStorage.state.isValid(payment)) {
          Left(payment)
        } else {
          Right(ValidationResult.NoBalance)
        }
      }
      case error: ValidationResult => Right(error)
    }
  }

  /**
    * Publish signed payment transaction which generated outside node
    */
  def broadcastPayment(payment: SignedPayment): Either[PaymentTransaction, ValidationResult] = {
    if (payment.fee < minimumTxFee)
      // TODO : add ValidationResult.InvalidFee to Scorex
      Right(ValidationResult.NegativeFee)
    else {
      val time = payment.timestamp
      val sigBytes = Base58.decode(payment.signature).get
      val senderPubKey = Base58.decode(payment.senderPublicKey).get
      val recipientAccount = new Account(payment.recipient)
      val tx = new PaymentTransaction(new PublicKeyAccount(senderPubKey),
        recipientAccount, payment.amount, payment.fee, time, Array.empty, sigBytes)

      tx.validate match {
        case ValidationResult.ValidateOke => {
          if (blockStorage.state.isValid(tx)) {
            onNewOffchainTransaction(tx)
            Left(tx)
          } else {
            Right(ValidationResult.NoBalance)
          }
        }
        case error: ValidationResult => Right(error)
      }
    }
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
