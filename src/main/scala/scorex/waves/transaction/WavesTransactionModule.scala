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
  def signPayment(payment: Payment, wallet: Wallet): Option[Either[ValidationResult,PaymentTransaction]] = {
    wallet.privateKeyAccount(payment.sender).map { sender =>
      PaymentTransaction.create(sender, new Account(payment.recipient), payment.amount, payment.fee, NTP.correctedTime())
    }
  }


  /**
    * Create signed payment transaction and validate it through current state.
    */
  def createSignedPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long, timestamp: Long): Either[ValidationResult, PaymentTransaction] = {
  
    val paymentVal = PaymentTransaction.create(sender, recipient, amount, fee, timestamp)

    paymentVal match {
      case Right(payment) => {
        if (blockStorage.state.isValid(payment, payment.timestamp)) {
          Right(payment)
        } else Left(ValidationResult.NoBalance)
      }
      case Left(err) => Left(err)
    }
  }

  /**
    * Publish signed payment transaction which generated outside node
    */
  def broadcastPayment(payment: SignedPayment): Either[ValidationResult, PaymentTransaction] = {
    val maybeSignatureBytes = Base58.decode(payment.signature).toOption
    if (payment.fee < minimumTxFee)
      Left(ValidationResult.InsufficientFee)
    else if (maybeSignatureBytes.isEmpty)
      Left(ValidationResult.InvalidSignature)
    else {
      val time = payment.timestamp
      val sigBytes = maybeSignatureBytes.get
      val senderPubKey = payment.senderPublicKey
      val recipientAccount = payment.recipient
      val txVal = PaymentTransaction.create(senderPubKey, recipientAccount, payment.amount, payment.fee, time, sigBytes)
      txVal match {
        case Right(tx) => {
          if (blockStorage.state.isValid(tx, tx.timestamp)) {
            onNewOffchainTransaction(tx)
            Right(tx)
          } else Left(ValidationResult.NoBalance)
        }
        case Left(err) => Left(err)
      }
    }
  }

  override def genesisData: BlockField[SimpleTransactionModule.StoredInBlock] = {
    TransactionsBlockField(chainParams.genesisTxs)
  }
}
