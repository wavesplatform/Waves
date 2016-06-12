package scorex.waves.transaction

import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.api.http.NoBalance
import scorex.app.Application
import scorex.block.BlockField
import scorex.crypto.encode.Base58
import scorex.settings.Settings
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.transaction.LagonakiTransaction.ValidationResult.ValidationResult
import scorex.transaction._
import scorex.utils.NTP

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
  def broadcastPayment(externalPayment: ExternalPayment): Either[PaymentTransaction, ValidationResult] = {
    val time = externalPayment.timestamp
    val sigBytes = Base58.decode(externalPayment.signature).get
    val senderPubKey = Base58.decode(externalPayment.senderPublicKey).get
    val recipientAccount = new Account(externalPayment.recipient)
    val payment = new PaymentTransaction(new PublicKeyAccount(senderPubKey),
      recipientAccount, externalPayment.amount, externalPayment.fee, time, sigBytes)

    payment.validate match {
      case ValidationResult.ValidateOke => {
        if (blockStorage.state.isValid(payment)) {
          onNewOffchainTransaction(payment)
          Left(payment)
        } else {
          Right(ValidationResult.NoBalance)
        }
      }
      case error: ValidationResult => Right(error)
    }
  }

  override def genesisData: BlockField[SimpleTransactionModule.StoredInBlock] = {


    val totalBalance = InitialBalance
    val txs = List(
      GenesisTransaction( new Account("3PAWwWa6GbwcJaFzwqXQN5KQm7H96Y7SHTQ"), totalBalance - 5 * UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("3P8JdJGYc7vaLu4UXUZc1iRLdzrkGtdCyJM"), UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("3PAGPDPqnGkyhcihyjMHe9v36Y4hkAh9yDy"), UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("3P9o3ZYwtHkaU1KxsKkFjJqJKS3dLHLC9oF"), UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("3PJaDyprvekvPXPuAtxrapacuDJopgJRaU3"), UnitsInWave, GenesisTransactionsTimestamp),
      GenesisTransaction( new Account("3PBWXDFUc86N2EQxKJmW8eFco65xTyMZx6J"), UnitsInWave, GenesisTransactionsTimestamp)
    )
    require(txs.foldLeft(0L)(_ + _.amount) == InitialBalance)


    TransactionsBlockField(txs)
  }
}
