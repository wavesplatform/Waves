package scorex.block

import java.math.BigDecimal
import java.math.BigInteger
import java.util.Arrays


import ntp.NTP

import org.json.simple.JSONArray
import org.json.simple.JSONObject

import scorex.BlockGenerator
import scorex.account.PublicKeyAccount
import scorex.crypto.Base58
import scorex.crypto.Crypto
import scorex.transaction.GenesisTransaction
import scorex.transaction.Transaction
import scorex.transaction.TransactionFactory

import com.google.common.primitives.Bytes
import com.google.common.primitives.Ints
import com.google.common.primitives.Longs

import database.DBSet

import scala.util.Try
import scala.collection.JavaConversions._
import scala.collection.JavaConverters._

object Block {
  val MAX_BLOCK_BYTES = 1048576
  val VERSION_LENGTH = 4
  val REFERENCE_LENGTH = 128
  val TIMESTAMP_LENGTH = 8
  val GENERATING_BALANCE_LENGTH = 8
  val GENERATOR_LENGTH = 32
  val GENERATOR_SIGNATURE_LENGTH = 64
  private[block] val TRANSACTIONS_SIGNATURE_LENGTH = 64
  private[block] val TRANSACTIONS_COUNT_LENGTH = 4
  private[block] val TRANSACTION_SIZE_LENGTH = 4
  private[block] val BASE_LENGTH = VERSION_LENGTH + REFERENCE_LENGTH + TIMESTAMP_LENGTH + GENERATING_BALANCE_LENGTH +
    GENERATOR_LENGTH + TRANSACTIONS_SIGNATURE_LENGTH + GENERATOR_SIGNATURE_LENGTH + TRANSACTIONS_COUNT_LENGTH
  val MAX_TRANSACTION_BYTES = MAX_BLOCK_BYTES - BASE_LENGTH

  def apply(stub:BlockStub, transactions: List[Transaction], transactionsSignature: Array[Byte]):Block =
    Block(stub.version, stub.reference, stub.timestamp,
      stub.generatingBalance, stub.generator, stub.generatorSignature,
      transactions, transactionsSignature)

  def parse(data: Array[Byte]): Try[Block] = Try {
    //CHECK IF WE HAVE MINIMUM BLOCK LENGTH
    require(data.length >= BASE_LENGTH, "Data is less then minimum block length")

    var position = 0

    //READ VERSION
    val versionBytes = Arrays.copyOfRange(data, position, position + VERSION_LENGTH)
    val version = Ints.fromByteArray(versionBytes)
    position += VERSION_LENGTH

    //READ TIMESTAMP
    val timestampBytes = Arrays.copyOfRange(data, position, position + TIMESTAMP_LENGTH)
    val timestamp = Longs.fromByteArray(timestampBytes)
    position += TIMESTAMP_LENGTH

    //READ REFERENCE
    val reference = Arrays.copyOfRange(data, position, position + REFERENCE_LENGTH)
    position += REFERENCE_LENGTH

    //READ GENERATING BALANCE
    val generatingBalanceBytes = Arrays.copyOfRange(data, position, position + GENERATING_BALANCE_LENGTH)
    val generatingBalance = Longs.fromByteArray(generatingBalanceBytes)
    position += GENERATING_BALANCE_LENGTH

    //READ GENERATOR
    val generatorBytes = Arrays.copyOfRange(data, position, position + GENERATOR_LENGTH)
    val generator = new PublicKeyAccount(generatorBytes)
    position += GENERATOR_LENGTH


    //READ TRANSACTION SIGNATURE
    val transactionsSignature = Arrays.copyOfRange(data, position, position + TRANSACTIONS_SIGNATURE_LENGTH)
    position += TRANSACTIONS_SIGNATURE_LENGTH

    //READ GENERATOR SIGNATURE
    val generatorSignature = Arrays.copyOfRange(data, position, position + GENERATOR_SIGNATURE_LENGTH)
    position += GENERATOR_SIGNATURE_LENGTH

    if (GenesisBlockParams.generatorSignature.sameElements(generatorSignature)){
      GenesisBlock
    }else {

      //READ TRANSACTIONS COUNT
      val transactionCountBytes = Arrays.copyOfRange(data, position, position + TRANSACTIONS_COUNT_LENGTH)
      val transactionCount = Ints.fromByteArray(transactionCountBytes)
      position += TRANSACTIONS_COUNT_LENGTH

      val (_, transactions) = (0 to transactionCount - 1).foldLeft((position, List[Transaction]())) { case ((pos, list), _) =>
        val transactionLengthBytes = Arrays.copyOfRange(data, pos, pos + TRANSACTION_SIZE_LENGTH)
        val transactionLength = Ints.fromByteArray(transactionLengthBytes)
        val transactionBytes = Arrays.copyOfRange(data, pos + TRANSACTION_SIZE_LENGTH, pos + TRANSACTION_SIZE_LENGTH + transactionLength)
        val transaction = TransactionFactory.getInstance().parse(transactionBytes)

        (position + TRANSACTION_SIZE_LENGTH + transactionLength, transaction :: list)
      }

      new Block(version, reference, timestamp, generatingBalance, generator, generatorSignature, transactions, transactionsSignature)
    }
  }
}

case class BlockStub(version: Int, reference: Array[Byte], timestamp: Long, generatingBalance: Long,
                 generator: PublicKeyAccount, generatorSignature: Array[Byte])

case class Block(version: Int, reference: Array[Byte], timestamp: Long, generatingBalance: Long,
                 generator: PublicKeyAccount, generatorSignature:  Array[Byte],
                 transactions:  List[Transaction], transactionsSignature:  Array[Byte]){

  import Block._

  //GETTERS/SETTERS

  def getTotalFee() = transactions.foldLeft(BigDecimal.ZERO.setScale(8)) { case (fee, tx) => fee.add(tx.getFee)}


  def getTransaction(signature: Array[Byte]) = transactions.find(tx => tx.getSignature.sameElements(signature))

  def getParent(): Block = getParent(DBSet.getInstance())

  def transactionsAsJava = transactions.asJava

  def getParent(db: DBSet): Block = db.getBlockMap().get(this.reference)


  def getChild(): Block = getChild(DBSet.getInstance())


  def getChild(db: DBSet): Block = db.getChildMap().get(this)

  def getHeight(): Int = getHeight(DBSet.getInstance())

  def getHeight(db: DBSet): Int = db.getHeightMap().get(this)

  lazy val signature = Bytes.concat(this.generatorSignature, this.transactionsSignature)

  //PARSE/CONVERT


  @SuppressWarnings(Array("unchecked"))
  def toJson() = {
    //todo: make it array not string
    val transactionsArray = JSONArray.toJSONString(transactions.map(tx => tx.toJson()))

    val fields = Map("version"->version,
      "reference" -> Base58.encode(reference),
      "timestamp" -> timestamp,
      "generatingBalance" -> generatingBalance,
      "generator" -> generator.getAddress,
      "fee" -> getTotalFee().toPlainString,
      "transactionsSignature" -> Base58.encode(transactionsSignature),
      "generatorSignature" -> Base58.encode(generatorSignature),
      "signature" -> Base58.encode(signature),
      "transactions" -> transactionsArray
    )
    new JSONObject(fields)
  }

  def toBytes = {
    val versionBytes = Ints.toByteArray(version)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), 8, 0)
    val referenceBytes = Bytes.ensureCapacity(reference, REFERENCE_LENGTH, 0)
    val baseTargetBytes = Longs.toByteArray(generatingBalance)
    val generatorBytes = Bytes.ensureCapacity(generator.getPublicKey, GENERATOR_LENGTH, 0)
    val transactionCountBytes = Ints.toByteArray(transactions.size)
    val transactionBytes = transactions.foldLeft(Array[Byte]()) { case (txBytes, tx) =>
      Bytes.concat(txBytes, Ints.toByteArray(tx.getDataLength), tx.toBytes)
    }

    Bytes.concat(versionBytes, timestampBytes, referenceBytes, baseTargetBytes, generatorBytes, transactionsSignature,
      generatorSignature, transactionCountBytes, transactionBytes)
  }

  def getDataLength() = transactions.foldLeft(BASE_LENGTH) { case (len, tx) => len + 4 + tx.getDataLength}

  //VALIDATE

  def isSignatureValid() = {
    val generatorSignature = Arrays.copyOfRange(reference, 0, GENERATOR_SIGNATURE_LENGTH)
    val baseTargetBytes = Longs.toByteArray(generatingBalance)
    val generatorBytes = Bytes.ensureCapacity(generator.getPublicKey, GENERATOR_LENGTH, 0)

    val blockSignature = Bytes.concat(generatorSignature, baseTargetBytes, generatorBytes)

    //VALIDATE TRANSACTIONS SIGNATURE
    lazy val txsSignature = transactions.foldLeft(generatorSignature) { case (sig, tx) =>
      Bytes.concat(sig, tx.getSignature)
    }

    Crypto.verify(generator.getPublicKey, generatorSignature, blockSignature) &&
      transactions.forall(_.isSignatureValid) &&
      Crypto.verify(this.generator.getPublicKey, transactionsSignature, txsSignature)
  }

  def isValid(): Boolean = isValid(DBSet.getInstance())


  def isValid(db: DBSet): Boolean = {
    //CHECK IF PARENT EXISTS
    if (this.reference == null || this.getParent(db) == null) {
      false
    } else if (this.timestamp - 500 > NTP.getTime || this.timestamp < this.getParent(db).timestamp) {
      //CHECK IF TIMESTAMP IS VALID -500 MS ERROR MARGIN TIME
      false
    } else if (this.timestamp % 1000 != this.getParent(db).timestamp % 1000) {
      //CHECK IF TIMESTAMP REST SAME AS PARENT TIMESTAMP REST
      false
    } else if (this.generatingBalance != BlockGenerator.getNextBlockGeneratingBalance(db, this.getParent(db))) {
      //CHECK IF GENERATING BALANCE IS CORRECT
      false
    } else {
      //CREATE TARGET
      val targetBytes = Array.fill(32)(Byte.MaxValue)

      //DIVIDE TARGET BY BASE TARGET
      val baseTarget = BigInteger.valueOf(BlockGenerator.getBaseTarget(this.generatingBalance))
      val target0 = new BigInteger(1, targetBytes)
        .divide(baseTarget)
        .multiply(generator.getGeneratingBalance(db).toBigInteger)

      //MULTIPLE TARGET BY GUESSES
      val guesses = (timestamp - getParent(db).timestamp) / 1000
      val lowerTarget = target0.multiply(BigInteger.valueOf(guesses - 1))
      val target = target0.multiply(BigInteger.valueOf(guesses))

      //CONVERT HIT TO BIGINT
      val hit = new BigInteger(1, Crypto.sha256(generatorSignature))

      if (hit.compareTo(target) >= 0) {
        false
      } else if (hit.compareTo(lowerTarget) < 0) {
        //CHECK IF FIRST BLOCK OF USER
        false
      } else {
        val fork = db.fork()
        transactions.forall { transaction =>
          !transaction.isInstanceOf[GenesisTransaction] &&
            transaction.isValid(fork) == Transaction.VALIDATE_OKE &&
            transaction.getTimestamp < timestamp && transaction.getDeadline >= timestamp
          //todo: investigate why it was in original source && transaction.process(fork)
        }
      }
    }
  }

  //PROCESS/ORPHAN

  def process(): Unit = process(DBSet.getInstance())

  def process(db: DBSet) {
    //PROCESS TRANSACTIONS
    transactions.foreach { transaction =>
      //PROCESS
      transaction.process(db)

      //SET PARENT
      db.getTransactionParentMap.set(transaction, this)

      //REMOVE FROM UNCONFIRMED DATABASE
      db.getTransactionMap.delete(transaction)
    }

    //PROCESS FEE
    val blockFee = this.getTotalFee()
    if (blockFee.compareTo(BigDecimal.ZERO) == 1) {
      //UPDATE GENERATOR BALANCE WITH FEE
      generator.setConfirmedBalance(generator.getConfirmedBalance(db).add(blockFee), db)
    }

    val parent = this.getParent(db)
    if (parent != null) {
      //SET AS CHILD OF PARENT
      db.getChildMap.set(parent, this)

      //SET BLOCK HEIGHT
      val height = parent.getHeight(db) + 1
      db.getHeightMap.set(this, height)
    } else {
      //IF NO PARENT HEIGHT IS 1
      db.getHeightMap.set(this, 1)
    }

    //ADD TO DB
    db.getBlockMap.add(this)

    //UPDATE LAST BLOCK
    db.getBlockMap.setLastBlock(this)
  }

  def orphan(): Unit = orphan(DBSet.getInstance())


  def orphan(db: DBSet) {
    //ORPHAN TRANSACTIONS
    orphanTransactions(transactions, db)

    //REMOVE FEE
    val blockFee = getTotalFee()
    if (blockFee.compareTo(BigDecimal.ZERO) == 1) {
      //UPDATE GENERATOR BALANCE WITH FEE
      generator.setConfirmedBalance(generator.getConfirmedBalance(db).subtract(blockFee), db)
    }

    //DELETE BLOCK FROM DB
    db.getBlockMap.delete(this)

    //SET PARENT AS LAST BLOCK
    db.getBlockMap.setLastBlock(getParent(db))

    //ADD ORPHANED TRANASCTIONS BACK TO DATABASE
    transactions.foreach(tx => db.getTransactionMap.add(tx))
  }

  private def orphanTransactions(transactions: List[Transaction], db: DBSet) =
  //ORPHAN ALL TRANSACTIONS IN DB BACK TO FRONT
    transactions.reverseIterator.foreach(_.orphan(db))
}
