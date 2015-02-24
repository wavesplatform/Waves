package scorex

import ntp.NTP
import controller.Controller
import scorex.account.Account
import scorex.account.PrivateKeyAccount
import scorex.account.PublicKeyAccount
import scorex.transaction.Transaction.ValidationResult
import scorex.transaction.Transaction.ValidationResult.ValidationResult
import scorex.transaction._


class TransactionCreator {
	def createPayment(sender:PrivateKeyAccount, recipient:Account,  amount:BigDecimal, fee:BigDecimal) = {
		val time = NTP.getTime
		val signature = PaymentTransaction.generateSignature(sender, recipient, amount, fee, time)
		val payment = new PaymentTransaction(new PublicKeyAccount(sender.publicKey), recipient, amount, fee, time, signature)
		this.afterCreate(payment)
	}
	
	private def afterCreate(transaction: Transaction): (Transaction, ValidationResult) = {
		val valid = transaction.isValid() //CHECK IF PAYMENT VALID
		if(valid == ValidationResult.VALIDATE_OKE){
			transaction.process()
			Controller.onTransactionCreate(transaction)
		}
		transaction -> valid
	}
}