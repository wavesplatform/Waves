package scorex.transaction

import scorex.controller.Controller
import scorex.ntp.NTP
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.Transaction.ValidationResult
import scorex.transaction.Transaction.ValidationResult.ValidationResult


object TransactionCreator {
  def createPayment(sender: PrivateKeyAccount,
                    recipient: Account,
                    amount: Long,
                    fee: Long): (Transaction, ValidationResult) = {
    val time = NTP.correctedTime()
    val signature = PaymentTransaction.generateSignature(sender, recipient, amount, fee, time)
    val payment = new PaymentTransaction(new PublicKeyAccount(sender.publicKey), recipient, amount, fee, time, signature)
    val valid = payment.isValid() //CHECK IF PAYMENT VALID
    if (valid == ValidationResult.VALIDATE_OKE) {
      Controller.onNewOffchainTransaction(payment)
    }
    payment -> valid
  }
}