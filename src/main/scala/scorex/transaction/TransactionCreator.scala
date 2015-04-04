package scorex.transaction

import controller.Controller
import ntp.NTP
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.transaction.Transaction.ValidationResult
import scorex.transaction.Transaction.ValidationResult.ValidationResult


object TransactionCreator {
  def createPayment(sender: PrivateKeyAccount,
                    recipient: Account,
                    amount: BigDecimal,
                    fee: BigDecimal): (Transaction, ValidationResult) = {
    val time = NTP.getTime
    val signature = PaymentTransaction.generateSignature(sender, recipient, amount, fee, time)
    val payment = new PaymentTransaction(new PublicKeyAccount(sender.publicKey), recipient, amount, fee, time, signature)
    val valid = payment.isValid() //CHECK IF PAYMENT VALID
    if (valid == ValidationResult.VALIDATE_OKE) {
      Controller.onTransactionCreate(payment)
    }
    payment -> valid
  }
}