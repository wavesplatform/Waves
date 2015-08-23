package scorex.block

import java.util.Arrays

import com.google.common.primitives.{Bytes, Ints, Longs}
import play.api.libs.json.{JsArray, JsObject, Json}
import scorex.account.{PrivateKeyAccount, PublicKeyAccount}
import scorex.app.Controller
import scorex.crypto.{Base58, SigningFunctionsImpl}
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.app.settings.Constants
import scorex.transaction.Transaction.ValidationResult
import scorex.transaction.{GenesisTransaction, Transaction}

import scala.util.Try

case class BlockStub(version: Byte,
                     reference: Array[Byte],
                     timestamp: Long,
                     generator: PrivateKeyAccount,
                     generationData: Constants.ConsensusAlgo.kernelData) {
  require(reference.length == Block.ReferenceLength)
}

case class Block(version: Byte,
                 reference: Array[Byte],
                 timestamp: Long,
                 generator: PublicKeyAccount,
                 generationData: Constants.ConsensusAlgo.kernelData,
                 transactions: Seq[Transaction],
                 signatureOpt: Option[Array[Byte]]) {

  import scorex.block.Block._

  lazy val totalFee = transactions.foldLeft(0L) { case (fee, tx) => fee + tx.fee }

  def parent(): Option[Block] = Controller.blockchainStorage.parent(this)

  def child(): Option[Block] = Controller.blockchainStorage.child(this)

  def height(): Option[Int] = Controller.blockchainStorage.heightOf(this)

  lazy val signature = signatureOpt.getOrElse {
    generator match {
      case privKeyAcc: PrivateKeyAccount =>
        SigningFunctionsImpl.sign(privKeyAcc, bytesWithoutSignature)

      case _ =>
        throw new IllegalStateException("Illegal piece of code reached, cant' sign block")
    }
  }

  lazy val json: JsObject =
    Json.obj("version" -> version.toInt,
      "signature" -> Base58.encode(signature),
      "reference" -> Base58.encode(reference),
      "timestamp" -> timestamp,
      "generator" -> generator.address,
      "fee" -> totalFee,
      "transactions" -> JsArray(transactions.map(_.json()))
    ) ++ generationData.json

  private lazy val bytesWithoutSignature = {
    val versionBytes = Array(version)
    val timestampBytes = Bytes.ensureCapacity(Longs.toByteArray(timestamp), 8, 0)
    val referenceBytes = Bytes.ensureCapacity(reference, ReferenceLength, 0)
    val transactionCountBytes = Ints.toByteArray(transactions.size)
    val transactionBytes = transactions.foldLeft(Array[Byte]()) { case (txBytes, tx) =>
      Bytes.concat(txBytes, Ints.toByteArray(tx.dataLength), tx.bytes())
    }

    Bytes.concat(versionBytes, timestampBytes, referenceBytes,
      Bytes.ensureCapacity(generator.publicKey, GeneratorLength, 0), generationData.bytes,
      transactionCountBytes, transactionBytes)
  }

  lazy val bytes = Bytes.concat(bytesWithoutSignature, signature)

  lazy val dataLength = transactions.foldLeft(BaseLength) { case (len, tx) => len + 4 + tx.dataLength }

  //VALIDATION

  lazy val signatureValid: Boolean = SigningFunctionsImpl.verify(signature, bytesWithoutSignature, generator.publicKey)

  def isValid(): Boolean = {
    if (reference == null || parent().isEmpty) {
      false
    } else if (this.timestamp < parent().get.timestamp) {
      false
    } else {
      generationData.isValid(this) &&
        transactions.forall { transaction =>
          !transaction.isInstanceOf[GenesisTransaction] &&
            transaction.validate() == ValidationResult.ValidateOke &&
            transaction.timestamp < timestamp && transaction.deadline >= timestamp
        }
    }
  }

  def process() = transactions.foreach(UnconfirmedTransactionsDatabaseImpl.remove)

  def rollback() = transactions.foreach(UnconfirmedTransactionsDatabaseImpl.putIfNew)
}


object Block {

  import Constants.ConsensusAlgo
  import ConsensusAlgo.kernelDataParser.GenerationDataLength

  val Version: Byte = 1
  val MaxBlockBytes = 1024 * 1024 // 1 mb block

  val VersionLength = 1
  val ReferenceLength = 64
  val TimestampLength = 8
  val GeneratorLength = 32

  private[block] val SignatureLength = scorex.crypto.SigningFunctionsImpl.SignatureLength
  private[block] val TransactionsCountLength = 4
  private[block] val TransactionSizeLength = 4
  private[block] val BaseLength = VersionLength + ReferenceLength + TimestampLength +
    GeneratorLength + GenerationDataLength + TransactionsCountLength + SignatureLength
  val MaxTransactionBytes = MaxBlockBytes - BaseLength

  def apply(stub: BlockStub, transactions: Seq[Transaction], account: PrivateKeyAccount): Block =
    Block(stub.version, stub.reference, stub.timestamp, stub.generator, stub.generationData, transactions, None)

  def apply(stub: BlockStub, account: PrivateKeyAccount): Block = {
    val orderedTransactions = UnconfirmedTransactionsDatabaseImpl.all().sortBy(_.feePerByte).toList

    val (_, transactions) = orderedTransactions.foldLeft((0, List[Transaction]())) {
      case ((totalBytes, filteredTxs), tx) =>
        if (tx.timestamp <= stub.timestamp && tx.deadline > stub.timestamp
          && tx.validate() == ValidationResult.ValidateOke
          && totalBytes + tx.dataLength <= Block.MaxTransactionBytes) {

          (totalBytes + tx.dataLength, tx :: filteredTxs)
        } else (totalBytes, filteredTxs)
    }

    Block(stub, transactions, account)
  }

  def parse(data: Array[Byte]): Try[Block] = Try {
    require(data.length >= BaseLength, "Data is less then minimum block length")

    var position = 0

    val version = data.head
    position += VersionLength

    val timestampBytes = Arrays.copyOfRange(data, position, position + TimestampLength)
    val timestamp = Longs.fromByteArray(timestampBytes)
    position += TimestampLength

    val reference = Arrays.copyOfRange(data, position, position + ReferenceLength)
    position += ReferenceLength

    val generatorBytes = Arrays.copyOfRange(data, position, position + GeneratorLength)
    val generator = new PublicKeyAccount(generatorBytes)
    position += GeneratorLength

    val generationDatabytes = Arrays.copyOfRange(data, position, position + GenerationDataLength)
    val generationData: ConsensusAlgo.kernelData = ConsensusAlgo.kernelDataParser.parse(generationDatabytes)
    position += GenerationDataLength

    if (generationData.isGenesis) {
      ConsensusAlgo.genesisBlock
    } else {

      val transactionCountBytes = Arrays.copyOfRange(data, position, position + TransactionsCountLength)
      val transactionCount = Ints.fromByteArray(transactionCountBytes)
      position += TransactionsCountLength

      val (sigPosition, transactions) = (1 to transactionCount).foldLeft((position, Seq[Transaction]())) { case ((pos, txs), _) =>
        val transactionLengthBytes = Arrays.copyOfRange(data, pos, pos + TransactionSizeLength)
        val transactionLength = Ints.fromByteArray(transactionLengthBytes)
        val transactionBytes = Arrays.copyOfRange(data, pos + TransactionSizeLength, pos + TransactionSizeLength + transactionLength)
        val transaction = Transaction.parse(transactionBytes)

        (pos + TransactionSizeLength + transactionLength, txs :+ transaction)
      }

      val signature = Arrays.copyOfRange(data, sigPosition, sigPosition + SignatureLength)

      new Block(version, reference, timestamp, generator, generationData, transactions, Some(signature))
    }
  }

  def isNewBlockValid(block: Block) =
    block != ConsensusAlgo.genesisBlock &&
      block.signatureValid &&
      Controller.blockchainStorage.lastBlock.signature.sameElements(block.reference) &&
      block.isValid()
}