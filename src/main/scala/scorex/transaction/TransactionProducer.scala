package scorex.transaction

import scorex.app.Controller
import scorex.app.utils.NTP
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.Transaction.ValidationResult


object TransactionProducer {
  def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): Transaction = {
    val time = NTP.correctedTime()
    val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, time)
    val payment = new PaymentTransaction(new PublicKeyAccount(sender.publicKey), recipient, amount, fee, time, sig)
    if (payment.validate() == ValidationResult.ValidateOke) {
      Controller.onNewOffchainTransaction(payment)
    }
    payment
  }
}