package scorex.wallet

import java.util.Arrays
import java.util.Observable
import java.util.Observer
import java.util.logging.Logger
import com.google.common.primitives.Bytes
import com.google.common.primitives.Ints

import controller.Controller
import database.wallet.SecureWalletDatabase
import database.wallet.WalletDatabase
import scorex.account.Account
import scorex.account.PrivateKeyAccount
import scorex.block.Block
import scorex.block.GenesisBlock
import scorex.crypto.Crypto
import scorex.transaction.Transaction
import utils.ObserverMessage
import scala.util.Try
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._


object Wallet extends Observable with Observer {
	val STATUS_UNLOCKED = 1
	val STATUS_LOCKED = 0
	
	private var database = if(exists()) new WalletDatabase() else null
	private var secureDatabase:SecureWalletDatabase = null
	
	//CONSTRUCTORS

	Controller.addObserver(this)

	//GETTERS/SETTERS
	
	
	def version() = database.getVersion

	
	def isUnlocked() = secureDatabase != null

	
	def getAccounts():List[Account] = database.getAccountMap().getAccounts.toList
	
	def accountExists(address:String) = database.getAccountMap().exists(address)

	
	def getAccount(address:String) = Option(database.getAccountMap().getAccount(address))

	
	def getUnconfirmedBalance(address:String) = database.getAccountMap.getUnconfirmedBalance(address)

	
	def getprivateKeyAccounts() = if(secureDatabase == null){
			List[PrivateKeyAccount]()
		}else {
			secureDatabase.synchronized{
				secureDatabase.getAccountSeedMap.getPrivateKeyAccounts.toList
			}
		}
	
	def getPrivateKeyAccount(address:String) = Option(secureDatabase).map(_.getAccountSeedMap.getPrivateKeyAccount(address))
	
	def exists() = WalletDatabase.exists()

	
	def getLastTransactions(limit:Int) =
		if(!this.exists()) List[utils.Pair[Account, Transaction]]().asJava
		else database.getTransactionMap.get(getAccounts(), limit)

	def getLastTransactions(account:Account, limit:Int) =
		if(!this.exists()) List[Transaction]().asJava
		else database.getTransactionMap.get(account, limit)

	
	def getLastBlocks():Seq[(Account, Block)] =
		if(!this.exists()) List()
		else database.getBlockMap.get(getAccounts()).map(pair => pair.getA -> pair.getB).toList


	def getLastBlocks(account:Account) =
		if(!this.exists()) List[Block]().asJava
		else database.getBlockMap.get(account)

	//CREATE
	
	def create(seed:Array[Byte], password:String, depth:Int, synchronize:Boolean):Boolean = {
		//OPEN WALLET
		val database = new WalletDatabase()
		
	    //OPEN SECURE WALLET
		val secureDatabase = new SecureWalletDatabase(password)
	    
	    //CREATE
	    create(database, secureDatabase, seed, depth, synchronize)
	}
	
	def create(database:WalletDatabase, secureDatabase:SecureWalletDatabase, seed:Array[Byte], depth:Int, sync:Boolean):Boolean = {
		//CREATE WALLET
		this.database = database
	    
	    //CREATE SECURE WALLET
	    this.secureDatabase = secureDatabase
	    
	    //ADD VERSION
	    database.setVersion(1)
	    
	    //ADD SEED
	    secureDatabase.setSeed(seed)
	    
	    //ADD NONCE
	    secureDatabase.setNonce(0)
	    
	    //CREATE ACCOUNTS
	    (1 to depth).foreach(_ => generateNewAccount())
	    
	    //SCAN TRANSACTIONS
	    if(sync) synchronize()

	    //COMMIT
	    commit()
	    
	    //ADD OBSERVER
	    Controller.addObserver(this)

	    true
	}
	
	def generateNewAccount():String = {
		//CHECK IF WALLET IS OPEN
		if(!this.isUnlocked())
		{
			return ""
		}
	    
	    //READ SEED
	    val seed = secureDatabase.getSeed
	    
	    //READ NONCE
	    val nonce = this.secureDatabase.getAndIncrementNonce
	    
	    //GENERATE ACCOUNT SEED
	    val accountSeed = this.generateAccountSeed(seed, nonce)
	    val account = new PrivateKeyAccount(accountSeed)
	    
	    //CHECK IF ACCOUNT ALREADY EXISTS
	    if(!this.accountExists(account.address))
	    {	    
	    	//ADD TO DATABASE
		    this.secureDatabase.getAccountSeedMap.add(account)
		    this.database.getAccountMap.add(account)
		    Logger.getGlobal.info("Added account #" + nonce)
		    
		    this.secureDatabase.commit()
		    this.database.commit()
		    
		    //NOTIFY
		    this.setChanged()
		    this.notifyObservers(new ObserverMessage(ObserverMessage.ADD_ACCOUNT_TYPE, account))
	    }
	    
	    account.address
	}
	
	def generateAccountSeed(seed:Array[Byte], nonce:Int) = {
		val nonceBytes = Ints.toByteArray(nonce)
		val accountSeed = Bytes.concat(nonceBytes, seed, nonceBytes)
		Crypto.doubleSha256(accountSeed)
	}
	
	//DELETE
	def deleteAccount(account:PrivateKeyAccount) = {
		//CHECK IF WALLET IS OPEN
		if(!isUnlocked()){
			false
		}else{
			//DELETE FROM DATABASE
			database.delete(account)
			secureDatabase.delete(account)

			//NOTIFY
			setChanged()
			notifyObservers(new ObserverMessage(ObserverMessage.REMOVE_ACCOUNT_TYPE, account))

			//RETURN
			true
		}
	}
	

	def synchronize(){
		val accounts = getAccounts()
		
		//RESET MAPS
		database.getTransactionMap.reset()
		database.getBlockMap.reset()
		Logger.getGlobal.info("Resetted maps")
		
		//REPROCESS BLOCKS
		var block:Block = GenesisBlock
		database.setLastBlockSignature(Array[Byte](1,1,1,1,1,1,1,1))
		do
		{
			//UPDATE
			this.update(this, new ObserverMessage(ObserverMessage.ADD_BLOCK_TYPE, block))
			
			if(block.getHeight() % 2000 == 0) 
			{
				Logger.getGlobal.info("Synchronize wallet: " + block.getHeight())
				this.database.commit()
			}
			
			//LOAD NEXT
			block = block.getChild()
		}
		while(block != null)
		
		//RESET UNCONFIRMED BALANCE
		accounts.synchronized{
			accounts.foreach(acc=> database.getAccountMap.update(acc, acc.getConfirmedBalance))
		}
		Logger.getGlobal.info("Resetted balances")
		
		//SET LAST BLOCK
		
		/*//SCAN TRANSACTIONS
		Map<Account, List<Transaction>> transactions
		synchronized(accounts)
		{
			transactions = Controller.scanTransactions(accounts)
		}
		
		//DELETE TRANSACTIONS
		this.database.getTransactionMap().deleteAll(accounts)
		
		//ADD TRANSACTIONS
		this.database.getTransactionMap().addAll(transactions)
	    	
		//TODO SCAN UNCONFIRMED TRANSACTIONS    
	    	    
	    //SCAN BLOCKS
	    Map<Account, List<Block>> blocks
	    synchronized(accounts)
		{
	    	blocks = Controller.scanBlocks(accounts)
		}
	    
	    //DELETE BLOCKS
	  	this.database.getBlockMap().deleteAll(accounts)
	  	
	  	//ADD BLOCKS
	  	this.database.getBlockMap().addAll(blocks)
	    
	   	    
	  	//TODO SCAN UNCONFIRMED NAMES

	  	//SET LAST BLOCK
	  	this.database.setLastBlockSignature(Controller.getLastBlock().getSignature())*/
	}
	
	//UNLOCK
	
	def unlock(password:String):Boolean =
	{
		if(isUnlocked()) {
			false
		}else{
			Try {
				val secureDatabase = new SecureWalletDatabase(password)
				unlock(secureDatabase)
			}.getOrElse(false)
		}
	}
	
	def unlock(secureDatabase:SecureWalletDatabase):Boolean = {
		this.secureDatabase = secureDatabase
		
		//NOTIFY
		setChanged()
		notifyObservers(new ObserverMessage(ObserverMessage.WALLET_STATUS, STATUS_UNLOCKED))
		
		true
	}
	
	def lock() = {
		if(!this.isUnlocked()){
			false
		}else {

			//CLOSE
			secureDatabase.close()
			secureDatabase = null

			//NOTIFY
			setChanged()
			notifyObservers(new ObserverMessage(ObserverMessage.WALLET_STATUS, STATUS_LOCKED))

			//LOCK SUCCESSFULL
			true
		}
	}

	//IMPORT/EXPORT
	
	def importAccountSeed(accountSeed:Array[Byte]) = {
		//CHECK IF WALLET IS OPEN
		if(!this.isUnlocked()){
			""
		}else if(accountSeed.length != 32){
			""
		}else {

			//CREATE ACCOUNT
			val account = new PrivateKeyAccount(accountSeed)

			//CHECK IF ACCOUNT ALREADY EXISTS
			if (!this.accountExists(account.address)) {
				//ADD TO DATABASE
				this.secureDatabase.getAccountSeedMap().add(account)
				this.database.getAccountMap().add(account)

				//SYNCHRONIZE
				this.synchronize

				//RETURN
				account.address
			} else {
				""
			}
		}
	}

	def exportAccountSeed(address:String):Option[Array[Byte]] =
		//CHECK IF WALLET IS OPEN
		if(!this.isUnlocked()) None
		else getPrivateKeyAccount(address).map(_.seed)


	def exportSeed():Array[Byte] = {
		//CHECK IF WALLET IS OPEN
		if(!this.isUnlocked()){
			null
		}else secureDatabase.getSeed()
	}
	
	//OBSERVER
	
	override def addObserver(o:Observer) {
		super.addObserver(o)
		
		//REGISTER ON ACCOUNTS
		database.getAccountMap().addObserver(o)
		
		//REGISTER ON TRANSACTIONS
		database.getTransactionMap().addObserver(o)
		
		//REGISTER ON BLOCKS
		database.getBlockMap().addObserver(o)

		//SEND STATUS
		var status = STATUS_LOCKED
		if(this.isUnlocked()){
			status = STATUS_UNLOCKED
		}
		
		o.update(this, new ObserverMessage(ObserverMessage.WALLET_STATUS, status))
	}

	private def processTransaction(transaction:Transaction){
		//CHECK IF WALLET IS OPEN
		if(this.exists()) {

			//FOR ALL ACCOUNTS
			val accounts = getAccounts
			accounts.synchronized {
				accounts.foreach{account =>
					//CHECK IF INVOLVED
					if (transaction.isInvolved(account)) {
						//ADD TO ACCOUNT TRANSACTIONS
						if (!database.getTransactionMap().add(account, transaction)) {
							//UPDATE UNCONFIRMED BALANCE
							val unconfirmedBalance = this.getUnconfirmedBalance(account.address).add(transaction.getAmount(account))
							database.getAccountMap().update(account, unconfirmedBalance)
						}
					}
				}
			}
		}
	}
	
	private def orphanTransaction(transaction:Transaction) {
		//CHECK IF WALLET IS OPEN
		if (exists()) {
			///FOR ALL ACCOUNTS
			val accounts = getAccounts()

			accounts.synchronized {
				accounts.foreach {account => 
					//CHECK IF INVOLVED
					if (transaction.isInvolved(account)) {
						//DELETE FROM ACCOUNT TRANSACTIONS
						database.getTransactionMap().delete(account, transaction)

						//UPDATE UNCONFIRMED BALANCE
						val unconfirmedBalance = this.getUnconfirmedBalance(account.address).subtract(transaction.getAmount(account))
						database.getAccountMap().update(account, unconfirmedBalance)
					}
				}
			}
		}
	}

	private def processBlock(block:Block){
		//CHECK IF WALLET IS OPEN
		if(exists()) {

			//CHECK IF WE NEED TO RESYNC
			val lastBlockSignature = database.getLastBlockSignature()
			if (lastBlockSignature == null || !Arrays.equals(lastBlockSignature, block.reference)) {
				Logger.getGlobal().info("Wallet not synchronized with current blockchain: synchronizing wallet.")
				this.synchronize()
			}

			//SET AS LAST BLOCK
			database.setLastBlockSignature(block.signature)

			//CHECK IF WE ARE GENERATOR
			if (accountExists(block.generator.address)) {
				//ADD BLOCK
				database.getBlockMap().add(block)

				//KEEP TRACK OF UNCONFIRMED BALANCE
				val unconfirmedBalance = getUnconfirmedBalance(block.generator.address).add(block.getTotalFee)
				database.getAccountMap().update(block.generator, unconfirmedBalance)
			}
		}
	}

	private def orphanBlock(block:Block){
		//CHECK IF WALLET IS OPEN
		if(exists()) {
			//CHECK IF WE ARE GENERATOR
			if (accountExists(block.generator.address)) {
				//DELETE BLOCK
				database.getBlockMap.delete(block)

				//KEEP TRACK OF UNCONFIRMED BALANCE
				val unconfirmedBalance = getUnconfirmedBalance(block.generator.address).subtract(block.getTotalFee)
				database.getAccountMap.update(block.generator, unconfirmedBalance)
			}
		}
	}
	
	override def update(o:Observable, arg:Object){
		val message = arg.asInstanceOf[ObserverMessage]
		
		if(message.getType == ObserverMessage.ADD_BLOCK_TYPE){
			val block = message.getValue.asInstanceOf[Block]
				
			//CHECK BLOCK
			processBlock(block)
				
			//CHECK TRANSACTIONS
			block.transactions.foreach(tx => processTransaction(tx))
		}
		
		if(message.getType == ObserverMessage.ADD_TRANSACTION_TYPE){	
			val transaction = message.getValue.asInstanceOf[Transaction]
			processTransaction(transaction)
		}
		
		if(message.getType == ObserverMessage.REMOVE_BLOCK_TYPE){
			val block = message.getValue().asInstanceOf[Block]
				
			//CHECK BLOCK
			orphanBlock(block)
				
			//CHECK TRANSACTIONS
			block.transactions.foreach(orphanTransaction)
		}
		
		if(message.getType == ObserverMessage.REMOVE_TRANSACTION_TYPE){
			val transaction = message.getValue.asInstanceOf[Transaction]
			orphanTransaction(transaction)
		}
	}

	//CLOSE
	
	def close(){
		if(database != null){
			database.close()
		}
		
		if(secureDatabase != null){
			secureDatabase.close()
		}
	}

	def commit(){
		if(database != null){
			database.commit()
		}
		
		if(secureDatabase != null){
			secureDatabase.commit()
		}
	}	
}