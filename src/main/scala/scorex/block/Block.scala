package scorex.block

import java.util.Arrays
import com.google.common.primitives.{Bytes, Ints, Longs}
import play.api.libs.json.{JsArray, JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.consensus.qora.{QoraBlockGenerationData}
import scorex.crypto.{Base58, Crypto}
import scorex.database.UnconfirmedTransactionsDatabaseImpl
import scorex.database.blockchain.PrunableBlockchainStorage
import scorex.transaction.Transaction.ValidationResult
import scorex.transaction.{GenesisTransaction, Transaction}
import scala.util.Try

case class BlockStub(version: Int, reference: Array[Byte], timestamp: Long,
                     generator: PublicKeyAccount, generationData: QoraBlockGenerationData) {
  require(reference.length == Block.REFERENCE_LENGTH)
}

case class Block(version: Int, reference: Array[Byte], timestamp: Long,
                 generator: PublicKeyAccount, generationData: QoraBlockGenerationData,
                 transactions: List[Transaction], transactionsSignature: Array[Byte]) {

  import scorex.block.Block._

  def totalFee() = transactions.foldLeft(BigDecimal(0).setScale(8)) { case (fee, tx) => fee + tx.fee }

  def getTransaction(signature: Array[Byte]) = transactions.find(tx => tx.signature.sameElements(signature))

  def parent(): Option[Block] = PrunableBlockchainStorage.parent(this)

  def child(): Option[Block] = PrunableBlockchainStorage.child(this)

  def height(): Option[Int] = PrunableBlockchainStorage.heightOf(this)

  lazy val signature = Bytes.concat(generationData.signature(), transactionsSignature)

  def toJson: JsObject =
    Json.obj("version" -> version,
      "signature" -> Base58.encode(signature),
      "reference" -> Base58.encode(reference),
      "timestamp" -> timestamp,
      "generator" -> generator.address,
      "fee" -> totalFee(),
      "transactionsSignature" -> Base58.encode(transactionsSignature),
      "transactions" -> JsArray(transactions.map(_.toJson()))
    ) ++ generationData.toJson

  def toBytes = {
    val versionBytes = Ints.toByteArray(version)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), 8, 0)
    val referenceBytes = Bytes.ensureCapacity(reference, REFERENCE_LENGTH, 0)
    val transactionCountBytes = Ints.toByteArray(transactions.size)
    val transactionBytes = transactions.foldLeft(Array[Byte]()) { case (txBytes, tx) =>
      Bytes.concat(txBytes, Ints.toByteArray(tx.dataLength), tx.toBytes())
    }

    Bytes.concat(versionBytes, timestampBytes, referenceBytes,
      Bytes.ensureCapacity(generator.publicKey, GENERATOR_LENGTH, 0), generationData.toBytes,
      transactionsSignature, transactionCountBytes, transactionBytes)
  }

  def dataLength() = transactions.foldLeft(BASE_LENGTH) { case (len, tx) => len + 4 + tx.dataLength }

  //VALIDATE

  def isSignatureValid() = {
    val txsSignature = transactions.foldLeft(generationData.signature()) { case (sig, tx) =>
      Bytes.concat(sig, tx.signature)
    }

    generationData.isSignatureValid(this) &&
      Crypto.verify(transactionsSignature, txsSignature, generator.publicKey) &&
      transactions.forall(_.isSignatureValid())
  }


  def isValid(): Boolean = {
    //CHECK IF PARENT EXISTS
    if (reference == null || parent().isEmpty) {
      false
    } else if (this.timestamp < parent().get.timestamp) {
      //CHECK IF TIMESTAMP IS VALID -500 MS ERROR MARGIN TIME
      false
    } else if (timestamp % 1000 != parent().get.timestamp % 1000) {
      //CHECK IF TIMESTAMP REST SAME AS PARENT TIMESTAMP REST
      false
    } else {
      generationData.isValid(this) &&
      transactions.forall { transaction =>
        !transaction.isInstanceOf[GenesisTransaction] &&
          transaction.isValid() == ValidationResult.VALIDATE_OKE &&
          transaction.timestamp < timestamp && transaction.deadline >= timestamp
      }
    }
  }

  def process() = transactions.foreach(UnconfirmedTransactionsDatabaseImpl.remove)

  def rollback() = transactions.foreach(UnconfirmedTransactionsDatabaseImpl.put)
}


object Block {
  val Version = 1
  val MAX_BLOCK_BYTES = 261120 //255 kb
  val VERSION_LENGTH = 4
  val REFERENCE_LENGTH = 128
  val TIMESTAMP_LENGTH = 8
  val GENERATOR_LENGTH = 32

  private[block] val TRANSACTIONS_SIGNATURE_LENGTH = 64
  private[block] val TRANSACTIONS_COUNT_LENGTH = 4
  private[block] val TRANSACTION_SIZE_LENGTH = 4
  private[block] val BASE_LENGTH = VERSION_LENGTH + REFERENCE_LENGTH + TIMESTAMP_LENGTH +
    GENERATOR_LENGTH + QoraBlockGenerationData.GENERATION_DATA_LENGTH +
    TRANSACTIONS_SIGNATURE_LENGTH + TRANSACTIONS_COUNT_LENGTH
  val MAX_TRANSACTION_BYTES = MAX_BLOCK_BYTES - BASE_LENGTH

  def apply(stub: BlockStub, transactions: List[Transaction], transactionsSignature: Array[Byte]): Block =
    Block(stub.version, stub.reference, stub.timestamp,
      stub.generator, stub.generationData, transactions, transactionsSignature)

  def apply(stub: BlockStub, account: PrivateKeyAccount): Block = {
    //ORDER TRANSACTIONS BY FEE PER BYTE
    val orderedTransactions = UnconfirmedTransactionsDatabaseImpl.getAll().sortBy(_.feePerByte)

    val (_, transactions) = orderedTransactions.foldLeft((0, List[Transaction]())) {
      case ((totalBytes, filteredTxs), tx) =>
        if (tx.timestamp <= stub.timestamp && tx.deadline > stub.timestamp
          && tx.isValid() == ValidationResult.VALIDATE_OKE
          && totalBytes + tx.dataLength <= Block.MAX_TRANSACTION_BYTES) {

          (totalBytes + tx.dataLength, tx :: filteredTxs)
        } else (totalBytes, filteredTxs)
    }

    val txSigsBytes = transactions.foldLeft(stub.generationData.signature()) { case (bytes, tx) =>
      Bytes.concat(bytes, tx.signature);
    }

    val transactionsSignature = Crypto.sign(account, txSigsBytes)
    Block(stub, transactions, transactionsSignature)
  }

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

    //READ GENERATOR
    val generatorBytes = Arrays.copyOfRange(data, position, position + GENERATOR_LENGTH)
    val generator = new PublicKeyAccount(generatorBytes)
    position += GENERATOR_LENGTH


    val generationDatabytes = Arrays.copyOfRange(data, position, position + QoraBlockGenerationData.GENERATION_DATA_LENGTH)
    val generationData = QoraBlockGenerationData.parse(generationDatabytes)
    position += QoraBlockGenerationData.GENERATION_DATA_LENGTH


    //READ TRANSACTION SIGNATURE
    val transactionsSignature = Arrays.copyOfRange(data, position, position + TRANSACTIONS_SIGNATURE_LENGTH)
    position += TRANSACTIONS_SIGNATURE_LENGTH

    if (generationData.isGenesis) {
      GenesisBlock
    } else {

      //READ TRANSACTIONS COUNT
      val transactionCountBytes = Arrays.copyOfRange(data, position, position + TRANSACTIONS_COUNT_LENGTH)
      val transactionCount = Ints.fromByteArray(transactionCountBytes)
      position += TRANSACTIONS_COUNT_LENGTH

      val (_, transactions) = (0 to transactionCount - 1).foldLeft((position, List[Transaction]())) { case ((pos, list), _) =>
        val transactionLengthBytes = Arrays.copyOfRange(data, pos, pos + TRANSACTION_SIZE_LENGTH)
        val transactionLength = Ints.fromByteArray(transactionLengthBytes)
        val transactionBytes = Arrays.copyOfRange(data, pos + TRANSACTION_SIZE_LENGTH, pos + TRANSACTION_SIZE_LENGTH + transactionLength)
        val transaction = Transaction.fromBytes(transactionBytes)

        (position + TRANSACTION_SIZE_LENGTH + transactionLength, transaction :: list)
      }

      new Block(version, reference, timestamp, generator, generationData, transactions, transactionsSignature)
    }
  }

  def isNewBlockValid(block: Block) =
    block != GenesisBlock &&
      block.isSignatureValid() &&
      PrunableBlockchainStorage.lastBlock.signature.sameElements(block.reference) &&
      block.isValid()
}