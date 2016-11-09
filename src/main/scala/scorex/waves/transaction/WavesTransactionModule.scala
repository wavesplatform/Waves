package scorex.waves.transaction

import com.wavesplatform.ChainParameters
import com.wavesplatform.settings.WavesSettings
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.app.RunnableApplication
import scorex.block.BlockField
import scorex.crypto.encode.Base58
import scorex.settings.Settings
import scorex.transaction.ValidationResult.ValidationResult
import scorex.transaction.{ValidationResult, _}
import scorex.transaction.state.wallet.Payment
import scorex.utils.NTP
import scorex.wallet.Wallet

/**
  * Waves Transaction Module
  */
class WavesTransactionModule(chainParams: ChainParameters)(implicit override val settings: TransactionSettings with Settings,
                             application: RunnableApplication)
  extends SimpleTransactionModule(chainParams) {

  override val InitialBalance = chainParams.initialBalance

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
    val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, timestamp)
    val payment = new PaymentTransaction(sender, recipient, amount, fee, timestamp, sig)
    payment
  }

  /**
    * Create signed payment transaction and validate it through current state.
    */
  def createSignedPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long, timestamp: Long): Either[ValidationResult, PaymentTransaction] = {
    val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, timestamp)
    val payment = new PaymentTransaction(sender, recipient, amount, fee, timestamp, sig)

    payment.validate match {
      case ValidationResult.ValidateOke => {
        if (blockStorage.state.isValid(payment, payment.timestamp)) {
          Right(payment)
        } else Left(ValidationResult.NoBalance)
      }
      case error: ValidationResult => Left(error)
    }
  }

  /**
    * Publish signed payment transaction which generated outside node
    */
  def broadcastPayment(payment: SignedPayment): Either[ValidationResult, PaymentTransaction] = {
    if (payment.fee < minimumTxFee)
      Left(ValidationResult.InsufficientFee)
    else {
      val time = payment.timestamp
      val sigBytes = Base58.decode(payment.signature).get
      val senderPubKey = Base58.decode(payment.senderPublicKey).get
      val recipientAccount = new Account(payment.recipient)
      val tx = new PaymentTransaction(new PublicKeyAccount(senderPubKey),
        recipientAccount, payment.amount, payment.fee, time, sigBytes)

      tx.validate match {
        case ValidationResult.ValidateOke => {
          if (blockStorage.state.isValid(tx, tx.timestamp)) {
            onNewOffchainTransaction(tx)
            Right(tx)
          } else Left(ValidationResult.NoBalance)
        }
        case error: ValidationResult.ValidationResult => Left(error)
      }
    }
  }

  override def genesisData: BlockField[SimpleTransactionModule.StoredInBlock] = {
    TransactionsBlockField(chainParams.genesisTxs)
  }
}
