package scorex.block

import java.util.Arrays
import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.controller.Controller
import play.api.libs.json.{JsArray, JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.{Base58, Crypto}
import scorex.database.UnconfirmedTransactionsDatabaseImpl
import scorex.transaction.Transaction.ValidationResult
import scorex.transaction.{GenesisTransaction, Transaction}
import scorex.settings.Constants
import scala.util.Try

case class BlockStub(version: Byte, reference: Array[Byte], timestamp: Long,
                     generator: PrivateKeyAccount, generationData: Constants.ConsensusAlgo.kernelData) {
  require(reference.length == Block.REFERENCE_LENGTH)
}

case class Block(version: Byte,
                 reference: Array[Byte],
                 timestamp: Long,
                 generator: PublicKeyAccount,
                 generationData: Constants.ConsensusAlgo.kernelData,
                 transactions: Seq[Transaction],
                 signatureOpt:Option[Array[Byte]]) {

  import scorex.block.Block._

  def totalFee() = transactions.foldLeft(0L) { case (fee, tx) => fee + tx.fee }

  def getTransaction(signature: Array[Byte]) = transactions.find(tx => tx.signature.sameElements(signature))

  def parent(): Option[Block] = Controller.blockchainStorage.parent(this)

  def child(): Option[Block] = Controller.blockchainStorage.child(this)

  def height(): Option[Int] = Controller.blockchainStorage.heightOf(this)

  lazy val signature = signatureOpt.getOrElse{
    generator match {
      case privKeyAcc:PrivateKeyAccount =>
        Crypto.sign(privKeyAcc, bytesWithoutSignature)

      case _ =>
        throw new IllegalStateException("Illegal piece of code reached, cant' sign block")
    }
  }

  def toJson: JsObject =
    Json.obj("version" -> version.toInt,
      "signature" -> Base58.encode(signature),
      "reference" -> Base58.encode(reference),
      "timestamp" -> timestamp,
      "generator" -> generator.address,
      "fee" -> totalFee(),
      "transactions" -> JsArray(transactions.map(_.toJson()))
    ) ++ generationData.toJson

  private lazy val bytesWithoutSignature = {
    val versionBytes = Array(version)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), 8, 0)
    val referenceBytes = Bytes.ensureCapacity(reference, REFERENCE_LENGTH, 0)
    val transactionCountBytes = Ints.toByteArray(transactions.size)
    val transactionBytes = transactions.foldLeft(Array[Byte]()) { case (txBytes, tx) =>
      Bytes.concat(txBytes, Ints.toByteArray(tx.dataLength), tx.toBytes())
    }

    Bytes.concat(versionBytes, timestampBytes, referenceBytes,
      Bytes.ensureCapacity(generator.publicKey, GENERATOR_LENGTH, 0), generationData.toBytes,
      transactionCountBytes, transactionBytes)
  }

  def toBytes = Bytes.concat(bytesWithoutSignature, signature)

  def dataLength() = transactions.foldLeft(BASE_LENGTH) { case (len, tx) => len + 4 + tx.dataLength }

  //VALIDATION

  def isSignatureValid() = Crypto.verify(signature, bytesWithoutSignature, generator.publicKey)

  def isValid(): Boolean = {
    //CHECK IF PARENT EXISTS
    if (reference == null || parent().isEmpty) {
      false
    } else if (this.timestamp < parent().get.timestamp) {
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

  def rollback() = transactions.foreach(UnconfirmedTransactionsDatabaseImpl.putIfNew)
}


object Block {

  import Constants.ConsensusAlgo
  import ConsensusAlgo.kernelDataParser.GENERATION_DATA_LENGTH

  val Version: Byte = 1
  val MAX_BLOCK_BYTES = 1024 * 1024 // 1 mb block

  val VERSION_LENGTH = 1
  val REFERENCE_LENGTH = 64
  val TIMESTAMP_LENGTH = 8
  val GENERATOR_LENGTH = 32

  private[block] val SIGNATURE_LENGTH = 64
  private[block] val TRANSACTIONS_COUNT_LENGTH = 4
  private[block] val TRANSACTION_SIZE_LENGTH = 4
  private[block] val BASE_LENGTH = VERSION_LENGTH + REFERENCE_LENGTH + TIMESTAMP_LENGTH +
    GENERATOR_LENGTH + GENERATION_DATA_LENGTH + TRANSACTIONS_COUNT_LENGTH + SIGNATURE_LENGTH
  val MAX_TRANSACTION_BYTES = MAX_BLOCK_BYTES - BASE_LENGTH

  def apply(stub: BlockStub, transactions: Seq[Transaction], account: PrivateKeyAccount): Block =
    Block(stub.version, stub.reference, stub.timestamp, stub.generator, stub.generationData, transactions, None)

  def apply(stub: BlockStub, account: PrivateKeyAccount): Block = {
    val orderedTransactions = UnconfirmedTransactionsDatabaseImpl.getAll().sortBy(_.feePerByte).toList

    val (_, transactions) = orderedTransactions.foldLeft((0, List[Transaction]())) {
      case ((totalBytes, filteredTxs), tx) =>
        if (tx.timestamp <= stub.timestamp && tx.deadline > stub.timestamp
          && tx.isValid() == ValidationResult.VALIDATE_OKE
          && totalBytes + tx.dataLength <= Block.MAX_TRANSACTION_BYTES) {

          (totalBytes + tx.dataLength, tx :: filteredTxs)
        } else (totalBytes, filteredTxs)
    }

    Block(stub, transactions, account)
  }

  def parse(data: Array[Byte]): Try[Block] = Try {
    require(data.length >= BASE_LENGTH, "Data is less then minimum block length")

    var position = 0

    val version = data.head
    position += VERSION_LENGTH

    val timestampBytes = Arrays.copyOfRange(data, position, position + TIMESTAMP_LENGTH)
    val timestamp = Longs.fromByteArray(timestampBytes)
    position += TIMESTAMP_LENGTH

    val reference = Arrays.copyOfRange(data, position, position + REFERENCE_LENGTH)
    position += REFERENCE_LENGTH

    val generatorBytes = Arrays.copyOfRange(data, position, position + GENERATOR_LENGTH)
    val generator = new PublicKeyAccount(generatorBytes)
    position += GENERATOR_LENGTH

    val generationDatabytes = Arrays.copyOfRange(data, position, position + GENERATION_DATA_LENGTH)
    val generationData: ConsensusAlgo.kernelData = ConsensusAlgo.kernelDataParser.parse(generationDatabytes)
    position += GENERATION_DATA_LENGTH

    if (generationData.isGenesis) {
      ConsensusAlgo.genesisBlock
    } else {

      val transactionCountBytes = Arrays.copyOfRange(data, position, position + TRANSACTIONS_COUNT_LENGTH)
      val transactionCount = Ints.fromByteArray(transactionCountBytes)
      position += TRANSACTIONS_COUNT_LENGTH

      val (sigPosition, transactions) = (1 to transactionCount).foldLeft((position, Seq[Transaction]())) { case ((pos, txs), _) =>
        val transactionLengthBytes = Arrays.copyOfRange(data, pos, pos + TRANSACTION_SIZE_LENGTH)
        val transactionLength = Ints.fromByteArray(transactionLengthBytes)
        val transactionBytes = Arrays.copyOfRange(data, pos + TRANSACTION_SIZE_LENGTH, pos + TRANSACTION_SIZE_LENGTH + transactionLength)
        val transaction = Transaction.parse(transactionBytes)

        (pos + TRANSACTION_SIZE_LENGTH + transactionLength, txs :+ transaction)
      }

      val signature = Arrays.copyOfRange(data, sigPosition, sigPosition + SIGNATURE_LENGTH)

      new Block(version, reference, timestamp, generator, generationData, transactions, Some(signature))
    }
  }

  def isNewBlockValid(block: Block) =
    block != ConsensusAlgo.genesisBlock &&
      block.isSignatureValid() &&
      Controller.blockchainStorage.lastBlock.signature.sameElements(block.reference) &&
      block.isValid()
}