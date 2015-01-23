package scorex

import java.math.BigDecimal
import java.math.BigInteger
import ntp.NTP
import scorex.account.PrivateKeyAccount
import scorex.block.{BlockStub, Block}
import scorex.crypto.Crypto
import scorex.transaction.Transaction
import com.google.common.primitives.Bytes
import com.google.common.primitives.Longs
import controller.Controller
import database.DBSet
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._
import scala.collection.concurrent.TrieMap


/**
 * Scala version of QORA's BlockGenerator
 *
 * There's one behavioral difference from original logic, please see comments to the
 * addUnconfirmedTransactions method. But it should be compatible with QORA.
 *
 *
 * kushti
 */

// TODO: make code more functional, i.e. get off of mutable variables, while(true) etc

object BlockGenerator extends Thread {
  val RETARGET = 10
  val MIN_BALANCE = 1L
  val MAX_BALANCE = 10000000000L
  val MIN_BLOCK_TIME = 1 * 60
  val MAX_BLOCK_TIME = 5 * 60

  private val blocks = TrieMap[PrivateKeyAccount, BlockStub]()
  private var solvingBlock: Block = _

  def addUnconfirmedTransaction(transaction: Transaction): Unit =
    addUnconfirmedTransaction(DBSet.getInstance(), transaction)

  def addUnconfirmedTransaction(db: DBSet, transaction: Transaction): Unit =
    db.getTransactionMap.add(transaction)

  def getUnconfirmedTransactions = DBSet.getInstance().getTransactionMap.getValues.toList.asJava //todo: fix after Controller rewriting

  private def getKnownAccounts = this.synchronized(Controller.getPrivateKeyAccounts) //todo: fix

  override def run() {
    while (true) {
      //CHECK IF WE ARE UPTODATE
      if (!Controller.isUpToDate()) Controller.update()


      //CHECK IF WE HAVE CONNECTIONS
      if (Controller.getStatus == Controller.STATUS_OKE) {
        val lastBlockSignature = DBSet.getInstance().getBlockMap.getLastBlockSignature

        //CHECK IF DIFFERENT FOR CURRENT SOLVING BLOCK
        if (this.solvingBlock == null || !this.solvingBlock.signature.sameElements(lastBlockSignature)) {
          //SET NEW BLOCK TO SOLVE
          this.solvingBlock = DBSet.getInstance().getBlockMap.getLastBlock

          //RESET BLOCKS
          this.blocks.clear()
        }

        //GENERATE NEW BLOCKS
        if (Controller.doesWalletExists()) {
          getKnownAccounts foreach { account =>
            if (account.getGeneratingBalance.compareTo(BigDecimal.ONE) >= 0) {
              //CHECK IF BLOCK FROM USER ALREADY EXISTS USE MAP ACCOUNT BLOCK EASY
              if (!blocks.containsKey(account)) {
                //GENERATE NEW BLOCK FOR USER
                blocks += account -> generateNextBlock(DBSet.getInstance(), account, solvingBlock)
              }
            }
          }
        }

        //IS VALID BLOCK FOUND?
        val validBlockFound = this.blocks.exists { case (account, blockStub) =>
          if (blockStub.timestamp <= NTP.getTime) {
            val block = formBlock(blockStub, DBSet.getInstance(), account)
            if(block.transactions.nonEmpty){
              println("Non-empty block: " + block)
            }
            Controller.newBlockGenerated(block)
          } else false
        }

        if (!validBlockFound) Thread.sleep(100)
      } else {
        Thread.sleep(100)
      }
    }
  }

  def generateNextBlock(db: DBSet, account: PrivateKeyAccount, block: Block) = {
    //CHECK IF ACCOUNT HAS BALANCE - but already checked before call (kushti)
    require(account.getGeneratingBalance(db) != BigDecimal.ZERO, "Zero balance in generateNextBlock")

    val signature = this.calculateSignature(db, block, account)
    val hash = Crypto.sha256(signature)
    val hashValue = new BigInteger(1, hash)

    //CALCULATE ACCOUNT TARGET
    val targetBytes = Array.fill(32)(Byte.MaxValue)
    val baseTarget = BigInteger.valueOf(getBaseTarget(getNextBlockGeneratingBalance(db, block)))
    val target = new BigInteger(1, targetBytes)
      .divide(baseTarget)
      .multiply(account.getGeneratingBalance(db).toBigInteger) //MULTIPLY TARGET BY USER BALANCE


    //CALCULATE GUESSES
    val guesses = hashValue.divide(target).add(BigInteger.ONE)

    //CALCULATE TIMESTAMP
    val timestampRaw = guesses.multiply(BigInteger.valueOf(1000)).add(BigInteger.valueOf(block.timestamp))

    //CHECK IF NOT HIGHER THAN MAX LONG VALUE
    val timestamp = (if (timestampRaw.compareTo(BigInteger.valueOf(Long.MaxValue)) == 1)
      BigInteger.valueOf(Long.MaxValue)
    else timestampRaw).longValue()

    val version = 1
    BlockStub(version, block.signature, timestamp, getNextBlockGeneratingBalance(db, block), account, signature)
  }

  def calculateSignature(db: DBSet, solvingBlock: Block, account: PrivateKeyAccount) = {
    //WRITE PARENT GENERATOR SIGNATURE
    val generatorSignature = Bytes.ensureCapacity(solvingBlock.generatorSignature, Block.GENERATOR_SIGNATURE_LENGTH, 0)

    //WRITE GENERATING BALANCE
    val baseTargetBytesRaw = Longs.toByteArray(getNextBlockGeneratingBalance(db, solvingBlock))
    val baseTargetBytes = Bytes.ensureCapacity(baseTargetBytesRaw, Block.GENERATING_BALANCE_LENGTH, 0)

    //WRITE GENERATOR
    val generatorBytes = Bytes.ensureCapacity(account.getPublicKey, Block.GENERATOR_LENGTH, 0)

    //CALC SIGNATURE OF NEWBLOCKHEADER
    Crypto.sign(account, Bytes.concat(generatorSignature, baseTargetBytes, generatorBytes))
  }



  def formBlock(stub: BlockStub, db: DBSet, account: PrivateKeyAccount): Block = {
    //CREATE FORK OF GIVEN DATABASE
    val newBlockDb = db.fork()

    //ORDER TRANSACTIONS BY FEE PER BYTE
    val orderedTransactions = db.getTransactionMap.getValues.toSeq.sortBy(_.feePerByte())

    /* warning: simplification here!
        QORA does break after first transaction matched conditions then repeat cycle
        (while orderedTransactions contains transactions to process)
     */
    val (_, transactions) = orderedTransactions.foldLeft((0, List[Transaction]())) {
      case ((totalBytes, filteredTxs), tx) =>
        if (tx.getTimestamp <= stub.timestamp && tx.getDeadline > stub.timestamp
          && tx.isValid(newBlockDb) == Transaction.VALIDATE_OKE
          && totalBytes + tx.getDataLength <= Block.MAX_TRANSACTION_BYTES) {

          tx.process(newBlockDb)
          (totalBytes + tx.getDataLength, tx :: filteredTxs)
        } else (totalBytes, filteredTxs)
    }

    val data = transactions.foldLeft(stub.generatorSignature) { case (bytes, tx) =>
      Bytes.concat(bytes, tx.getSignature);
    }
    val transactionsSingature = Crypto.sign(account, data)

    Block(stub, transactions, transactionsSingature)
  }

  def getNextBlockGeneratingBalance(db: DBSet, block: Block) = {
    if (block.getHeight(db) % RETARGET == 0) {
      //GET FIRST BLOCK OF TARGET
      val firstBlock = (1 to RETARGET - 1).foldLeft(block) { case (bl, _) => bl.getParent(db)}

      //CALCULATE THE GENERATING TIME FOR LAST 10 BLOCKS
      val generatingTime = block.timestamp - firstBlock.timestamp

      //CALCULATE EXPECTED FORGING TIME
      val expectedGeneratingTime = getBlockTime(block.generatingBalance) * RETARGET * 1000

      //CALCULATE MULTIPLIER
      val multiplier = expectedGeneratingTime / generatingTime.toDouble

      //CALCULATE NEW GENERATING BALANCE
      val generatingBalance = (block.generatingBalance * multiplier).toLong
      minMaxBalance(generatingBalance)
    } else block.generatingBalance
  }

  def getBaseTarget(generatingBalance: Long) = minMaxBalance(generatingBalance) * getBlockTime(generatingBalance)

  def getBlockTime(generatingBalance: Long) = {
    val percentageOfTotal = minMaxBalance(generatingBalance) / MAX_BALANCE.toDouble
    (MIN_BLOCK_TIME + ((MAX_BLOCK_TIME - MIN_BLOCK_TIME) * (1 - percentageOfTotal))).toLong
  }

  def minMaxBalance(generatingBalance: Long) =
    if (generatingBalance < MIN_BALANCE) MIN_BALANCE
    else if (generatingBalance > MAX_BALANCE) MAX_BALANCE
    else generatingBalance
}