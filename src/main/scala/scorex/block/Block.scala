package scorex.block

import java.util.Arrays

import com.google.common.primitives.{Bytes, Ints, Longs}
import ntp.NTP
import play.api.libs.json.{JsArray, JsObject, Json}
import scorex.BlockGenerator
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.{Base58, Crypto}
import scorex.database.{PrunableBlockchainStorage, UnconfirmedTransactionsDatabaseImpl}
import scorex.transaction.Transaction.ValidationResult
import scorex.transaction.{GenesisTransaction, Transaction}
import scala.util.Try

case class BlockStub(version: Int, reference: Array[Byte], timestamp: Long, generatingBalance: Long,
                     generator: PublicKeyAccount, generatorSignature: Array[Byte]) {
  require(reference.length == Block.REFERENCE_LENGTH)
  require(generatingBalance > 0)
}

case class Block(version: Int, reference: Array[Byte], timestamp: Long, generatingBalance: Long,
                 generator: PublicKeyAccount, generatorSignature: Array[Byte],
                 transactions: List[Transaction], transactionsSignature: Array[Byte]) {

  import scorex.block.Block._

  def totalFee() = transactions.foldLeft(BigDecimal(0).setScale(8)) { case (fee, tx) => fee + tx.fee}

  def getTransaction(signature: Array[Byte]) = transactions.find(tx => tx.signature.sameElements(signature))

  def parent(): Option[Block] = PrunableBlockchainStorage.parent(this)

  def child(): Option[Block] = PrunableBlockchainStorage.child(this)

  def height(): Option[Int] = PrunableBlockchainStorage.heightOf(this)

  lazy val signature = Bytes.concat(generatorSignature, transactionsSignature)

  def toJson: JsObject =
    Json.obj("version" -> version,
      "reference" -> Base58.encode(reference),
      "timestamp" -> timestamp,
      "generatingBalance" -> generatingBalance,
      "generator" -> generator.address,
      "fee" -> totalFee(),
      "transactionsSignature" -> Base58.encode(transactionsSignature),
      "generatorSignature" -> Base58.encode(generatorSignature),
      "signature" -> Base58.encode(signature),
      "transactions" -> JsArray(transactions.map(_.toJson()))
    )

  def toBytes = {
    val versionBytes = Ints.toByteArray(version)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), 8, 0)
    val referenceBytes = Bytes.ensureCapacity(reference, REFERENCE_LENGTH, 0)
    val baseTargetBytes = Longs.toByteArray(generatingBalance)
    val generatorBytes = Bytes.ensureCapacity(generator.publicKey, GENERATOR_LENGTH, 0)
    val transactionCountBytes = Ints.toByteArray(transactions.size)
    val transactionBytes = transactions.foldLeft(Array[Byte]()) { case (txBytes, tx) =>
      Bytes.concat(txBytes, Ints.toByteArray(tx.dataLength), tx.toBytes())
    }

    Bytes.concat(versionBytes, timestampBytes, referenceBytes, baseTargetBytes, generatorBytes, transactionsSignature,
      generatorSignature, transactionCountBytes, transactionBytes)
  }

  def dataLength() = transactions.foldLeft(BASE_LENGTH) { case (len, tx) => len + 4 + tx.dataLength}

  //VALIDATE

  def isSignatureValid() = {
    val baseTargetBytes = Longs.toByteArray(generatingBalance).ensuring(_.size == Block.GENERATING_BALANCE_LENGTH)

    require(generator.publicKey.size == GENERATOR_LENGTH)
    val blockSignature = Bytes.concat(Arrays.copyOfRange(reference, 0, GENERATOR_SIGNATURE_LENGTH),
      baseTargetBytes,
      generator.publicKey)

    //VALIDATE TRANSACTIONS SIGNATURE
    val txsSignature = transactions.foldLeft(generatorSignature) { case (sig, tx) =>
      Bytes.concat(sig, tx.signature)
    }

    Crypto.verify(generatorSignature, blockSignature, generator.publicKey) &&
      Crypto.verify(transactionsSignature, txsSignature, generator.publicKey) &&
      transactions.forall(_.isSignatureValid())
  }


  def isValid(): Boolean = {
    //CHECK IF PARENT EXISTS
    if (reference == null || parent().isEmpty) {
      false
    } else if (timestamp - 500 > NTP.getTime || this.timestamp < parent().get.timestamp) {
      //CHECK IF TIMESTAMP IS VALID -500 MS ERROR MARGIN TIME
      false
    } else if (timestamp % 1000 != parent().get.timestamp % 1000) {
      //CHECK IF TIMESTAMP REST SAME AS PARENT TIMESTAMP REST
      false
    } else if (generatingBalance != BlockGenerator.getNextBlockGeneratingBalance(parent().get)) {
      //CHECK IF GENERATING BALANCE IS CORRECT
      false
    } else {
      //CREATE TARGET
      val targetBytes = Array.fill(32)(Byte.MaxValue)

      //DIVIDE TARGET BY BASE TARGET
      val baseTarget = BigInt(BlockGenerator.getBaseTarget(generatingBalance))
      val genBalance = PrunableBlockchainStorage.generationBalance(generator.address).toBigInt()
      val target0 = BigInt(1, targetBytes) / baseTarget * genBalance

      //MULTIPLE TARGET BY GUESSES
      val guesses = (timestamp - parent().get.timestamp) / 1000
      val lowerTarget = target0 * BigInt(guesses - 1)
      val target = target0 * BigInt(guesses)

      //CONVERT HIT TO BIGINT
      val hit = BigInt(1, Crypto.sha256(generatorSignature))

      if (hit >= target) {
        false
      } else if (hit < lowerTarget) {
        //CHECK IF FIRST BLOCK OF USER
        false
      } else {
        transactions.forall { transaction =>
          !transaction.isInstanceOf[GenesisTransaction] &&
            transaction.isValid() == ValidationResult.VALIDATE_OKE &&
            transaction.timestamp < timestamp && transaction.deadline >= timestamp
        }
      }
    }
  }

  def process() {
    transactions.foreach { transaction =>
      UnconfirmedTransactionsDatabaseImpl.remove(transaction)
    }
  }

  def rollback() {
    transactions.foreach(UnconfirmedTransactionsDatabaseImpl.put)
  }
}


object Block {
  val Version = 1
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

  def apply(stub: BlockStub, transactions: List[Transaction], transactionsSignature: Array[Byte]): Block =
    Block(stub.version, stub.reference, stub.timestamp,
      stub.generatingBalance, stub.generator, stub.generatorSignature,
      transactions, transactionsSignature)

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

    val txSigsBytes = transactions.foldLeft(stub.generatorSignature) { case (bytes, tx) =>
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

    if (GenesisBlockParams.generatorSignature.sameElements(generatorSignature)) {
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

      new Block(version, reference, timestamp, generatingBalance, generator, generatorSignature, transactions, transactionsSignature)
    }
  }

  def isNewBlockValid(block: Block) =
    block != GenesisBlock &&
      block.isSignatureValid() &&
      PrunableBlockchainStorage.lastBlock.signature.sameElements(block.reference) &&
      block.isValid()

  def calculateSignature(solvingBlock: Block, account: PrivateKeyAccount) = {
    //WRITE PARENT GENERATOR SIGNATURE
    val generatorSignature = solvingBlock.generatorSignature

    //WRITE GENERATING BALANCE
    val baseTargetBytes = Longs.toByteArray(BlockGenerator.getNextBlockGeneratingBalance(solvingBlock)).ensuring(_.size == Block.GENERATING_BALANCE_LENGTH)

    //CALC SIGNATURE OF NEWBLOCKHEADER
    require(generatorSignature.size == Block.GENERATOR_SIGNATURE_LENGTH)
    require(account.publicKey.size == Block.GENERATOR_LENGTH)
    Crypto.sign(account, Bytes.concat(generatorSignature, baseTargetBytes, account.publicKey))
  }
}