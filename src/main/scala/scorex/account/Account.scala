package scorex.account

import java.math.BigDecimal
import java.util.Arrays

import controller.Controller
import scorex.BlockGenerator
import scorex.block.Block
import database.DBSet

class Account(val address:String) {
	private var lastBlockSignatureOpt:Option[Array[Byte]] = None
	private var generatingBalance = BigDecimal.ZERO.setScale(8)

	//BALANCE

	def unconfirmedBalance() = Controller.getUnconfirmedBalance(address)

	def getConfirmedBalance():BigDecimal = getConfirmedBalance(DBSet.getInstance())

	def getConfirmedBalance(db:DBSet):BigDecimal = db.getBalanceMap().get(address)

	
	def getConfirmedBalance(key:Long):BigDecimal = getConfirmedBalance(key, DBSet.getInstance())
	
	
	def getConfirmedBalance(key:Long, db:DBSet):BigDecimal = db.getBalanceMap().get(address, key)
	

	def setConfirmedBalance(amount:BigDecimal){
		setConfirmedBalance(amount, DBSet.getInstance())
	}
	
	def setConfirmedBalance(amount:BigDecimal, db:DBSet){
		//UPDATE BALANCE IN DB
		db.getBalanceMap().set(address, amount)
	}
	
	def setConfirmedBalance(key:Long, amount:BigDecimal) {
		setConfirmedBalance(key, amount, DBSet.getInstance())
	}
	
	def setConfirmedBalance(key:Long, amount:BigDecimal, db:DBSet){
		//UPDATE BALANCE IN DB
		db.getBalanceMap().set(address, key, amount)
	}
	
	def getBalance(confirmations:Int):BigDecimal = getBalance(confirmations, DBSet.getInstance())

	def getBalance(confirmations:Int, db:DBSet):BigDecimal = confirmations match{
		//CHECK IF UNCONFIRMED BALANCE
		case i:Int if i <= 0 => unconfirmedBalance()
		case 1 => getConfirmedBalance(db)
		case _ =>
			//GO TO PARENT BLOCK 10
			val block = db.getBlockMap.getLastBlock
			(1 to confirmations-1).foldLeft((getConfirmedBalance(db),Option(block))){
				case ((blnc, blckOpt), _) =>
					blckOpt match {
						case Some(blck) if blck.isInstanceOf[Block] =>
							val updBlnc = block.transactions.foldLeft(blnc){case (b, tx) =>
								  if(tx.isInvolved(this)) b.subtract(tx.getAmount(this)) else b
							}
							(updBlnc, Option(blck.getParent(db)))
						case _ => (blnc,None)
					}
			}._1
	}

	def calculateGeneratingBalance(db:DBSet) {
		//CONFIRMED BALANCE + ALL NEGATIVE AMOUNTS IN LAST 9 BLOCKS
		val start = (getConfirmedBalance(db), Option(db.getBlockMap().getLastBlock()))

		val balance = (1 to BlockGenerator.RETARGET-1).foldLeft(start){case ((blnc, blckOpt),_) =>
			  blckOpt match{
					case Some(blck) if blck.getHeight(db) > 1 =>
						val updBlnc = blck.transactions.foldLeft(blnc){case (b, tx) =>
							  if(tx.isInvolved(this) && tx.getAmount(this).compareTo(BigDecimal.ZERO) == 1){
									b.subtract(tx.getAmount(this))
								} else b
						}
						(blnc, Option(blck.getParent(db)))

					case _ => (blnc, blckOpt)
				}
		}._1

		//DO NOT GO BELOW 0
		generatingBalance = if(balance.compareTo(BigDecimal.ZERO) == -1)
			BigDecimal.ZERO.setScale(8)
		else balance
	}
	
	def getGeneratingBalance():BigDecimal = getGeneratingBalance(DBSet.getInstance())

	def getGeneratingBalance(db:DBSet):BigDecimal = {
		//CHECK IF WE NEED TO RECALCULATE
		lastBlockSignatureOpt match{
			case None =>
				lastBlockSignatureOpt = Some(db.getBlockMap().getLastBlockSignature())
				calculateGeneratingBalance(db)

			case Some(lastBlockSignature) =>
				//CHECK IF WE NEED TO RECALCULATE
				if(!Arrays.equals(lastBlockSignature, db.getBlockMap().getLastBlockSignature())){
					lastBlockSignatureOpt = Some(db.getBlockMap().getLastBlockSignature())
					calculateGeneratingBalance(db)
				}
		}
		generatingBalance
	}

	
	//REFERENCE
	
	def getLastReference():Array[Byte] = getLastReference(DBSet.getInstance())
	
	def getLastReference(db:DBSet):Array[Byte] = db.getReferenceMap.get(this)
	
	def setLastReference(reference:Array[Byte]){
		setLastReference(reference, DBSet.getInstance())
	}

	def setLastReference(reference:Array[Byte], db:DBSet) {
		db.getReferenceMap().set(this, reference)
	}
	
	def removeReference() {
		removeReference(DBSet.getInstance())
	}
	
	def removeReference(db:DBSet) {
		db.getReferenceMap().delete(this)
	}
	
	//TOSTRING
	
	override def toString() = getBalance(0).toPlainString() + " - " + address
	
	def toString(key:Long) = getConfirmedBalance(key).toPlainString + " - " + address
	
	//EQUALS
	override def equals(b:Any) = b match {
		case a:Account => a.address == address
		case _ => false
	}
}


object Account {
	val ADDRESS_LENGTH = 25
}