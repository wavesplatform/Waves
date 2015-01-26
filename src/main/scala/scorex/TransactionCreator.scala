package scorex

import java.math.BigDecimal
import ntp.NTP
import controller.Controller
import scorex.account.Account
import scorex.account.PrivateKeyAccount
import scorex.account.PublicKeyAccount
import scorex.block.Block
import scorex.transaction.Transaction.ValidationResult
import scorex.transaction.Transaction.ValidationResult
import scorex.transaction.Transaction.ValidationResult.ValidationResult
import scorex.transaction._
import utils.Pair
import database.DBSet
import scala.collection.JavaConversions._


class TransactionCreator {
	private var fork:DBSet = _
	private var lastBlock:Block = _
	
	private def checkUpdate() {
		//CHECK IF WE ALREADY HAVE A FORK
		Option(lastBlock) match {
			case None => updateFork() 
			case Some(_) => if(this.lastBlock.signature.sameElements(Controller.getLastBlock.signature))
				{
					updateFork()
				}
		}
	}
	
	private def updateFork()
	{
		//CREATE NEW FORK
		this.fork = DBSet.getInstance().fork()
		
		//UPDATE LAST BLOCK
		this.lastBlock = Controller.getLastBlock
			
		//SCAN UNCONFIRMED TRANSACTIONS FOR TRANSACTIONS WHERE ACCOUNT IS CREATOR OF
		//& SORT THEM BY TIMESTAMP
		val accountTransactions = DBSet.getInstance().getTransactionMap.getTransactions.filter{
			transaction =>
				Controller.getAccounts.contains(transaction.getCreator)
		}.sortBy(_.timestamp)
			
		//VALIDATE AND PROCESS THOSE TRANSACTIONS IN FORK
		accountTransactions foreach {transaction =>
			if(transaction.isValid(this.fork) == ValidationResult.VALIDATE_OKE && transaction.isSignatureValid){
				transaction.process(this.fork)
			}else{
				DBSet.getInstance().getTransactionMap.delete(transaction) //THE TRANSACTION BECAME INVALID LET
			}
		}
	}
	
	
	def createPayment(sender:PrivateKeyAccount, recipient:Account,  amount:BigDecimal, fee:BigDecimal) = {
		this.checkUpdate()
		val time = NTP.getTime
		val signature = PaymentTransaction.generateSignature(this.fork, sender, recipient, amount, fee, time)
		val payment = new PaymentTransaction(new PublicKeyAccount(sender.publicKey), recipient, amount, fee, time, sender.getLastReference(this.fork), signature)
		this.afterCreate(payment)
	}
	
	private def afterCreate(transaction: Transaction): (Transaction, ValidationResult) = {
		val valid = transaction.isValid(this.fork) //CHECK IF PAYMENT VALID
		if(valid == ValidationResult.VALIDATE_OKE){
			transaction.process(this.fork)
			Controller.onTransactionCreate(transaction)
		}
		transaction -> valid
	}
}