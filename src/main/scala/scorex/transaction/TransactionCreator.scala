package scorex.transaction

import scorex.controller.Controller
import scorex.ntp.NTP
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.Transaction.ValidationResult


object TransactionCreator {
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