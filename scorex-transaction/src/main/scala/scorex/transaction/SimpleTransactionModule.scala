package scorex.transaction

import com.google.common.primitives.{Bytes, Ints}
import play.api.libs.json.{JsObject, Json}
import scorex.account.Account
import scorex.block.{Block, BlockField}
import scorex.consensus.ConsensusModule
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.transaction.SimpleTransactionModule.StoredInBlock
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.transaction.state.database.blockchain.{StoredBlockTree, StoredBlockchain, StoredState}
import scorex.utils.ScorexLogging

import scala.concurrent.duration._
import scala.util.Try

case class TransactionsBlockField(override val value: Seq[Transaction])
  extends BlockField[Seq[Transaction]] {

  import SimpleTransactionModule.MaxTransactionsPerBlock

  override val name = "transactions"

  override lazy val json: JsObject = Json.obj(name -> Json.arr(value.map(_.json())))

  override lazy val bytes: Array[Byte] = {
    val txCount = value.size.ensuring(_ <= MaxTransactionsPerBlock).toByte
    value.foldLeft(Array(txCount)) { case (bs, tx) =>
      val txBytes = tx.bytes()
      bs ++ Bytes.ensureCapacity(Ints.toByteArray(txBytes.length), 4, 0) ++ txBytes
    }
  }
}

class SimpleTransactionModule(implicit val settings: TransactionSettings,
                              consensusModule: ConsensusModule[_])
  extends TransactionModule[StoredInBlock] with ScorexLogging {

  import SimpleTransactionModule._

  val TransactionSizeLength = 4

  private val instance = this

  override val blockStorage = new BlockStorage {
    override val history: History = settings.history match {
      case s: String if s.equalsIgnoreCase("blockchain") =>
        new StoredBlockchain(settings.dataDirOpt)(consensusModule, instance)
      case s: String if s.equalsIgnoreCase("blocktree") =>
        new StoredBlockTree(settings.dataDirOpt, settings.MaxRollback)(consensusModule, instance)
      case s =>
        log.error(s"Unknown history storage: $s. Use StoredBlockchain...")
        new StoredBlockchain(settings.dataDirOpt)(consensusModule, instance)
    }
    override val state = new StoredState(settings.dataDirOpt)
  }

  /**
    * In Lagonaki, transaction-related data is just sequence of transactions. No Merkle-tree root of txs / state etc
    * @param bytes - serialized sequence of transaction
    * @return
    */
  override def parseBlockData(bytes: Array[Byte]): Try[TransactionsBlockField] = Try {
    bytes.isEmpty match {
      case true => TransactionsBlockField(Seq())
      case false =>
        val txData = bytes.tail
        val txCount = bytes.head // so 255 txs max
        formBlockData((1 to txCount).foldLeft((0: Int, Seq[LagonakiTransaction]())) { case ((pos, txs), _) =>
          val transactionLengthBytes = txData.slice(pos, pos + TransactionSizeLength)
          val transactionLength = Ints.fromByteArray(transactionLengthBytes)
          val transactionBytes = txData.slice(pos + TransactionSizeLength, pos + TransactionSizeLength + transactionLength)
          val transaction = LagonakiTransaction.parse(transactionBytes).get

          (pos + TransactionSizeLength + transactionLength, txs :+ transaction)
        }._2)
    }
  }

  override def formBlockData(transactions: StoredInBlock): TransactionsBlockField =
    TransactionsBlockField(transactions)

  override def transactions(block: Block): StoredInBlock =
    block.transactionDataField.asInstanceOf[TransactionsBlockField].value //todo: asInstanceOf

  override def packUnconfirmed(): StoredInBlock =
    UnconfirmedTransactionsDatabaseImpl.all().filter(isValid(_)).filter(!blockStorage.state.included(_))

  //todo: check: clear unconfirmed txs on receiving a block
  override def clearFromUnconfirmed(data: StoredInBlock): Unit = {
    data.foreach(tx => UnconfirmedTransactionsDatabaseImpl.getBySignature(tx.signature) match {
      case Some(unconfirmedTx) => UnconfirmedTransactionsDatabaseImpl.remove(unconfirmedTx)
      case None =>
    })

    val lastBlockTs = blockStorage.history.lastBlock.timestampField.value
    UnconfirmedTransactionsDatabaseImpl.all().foreach { tx =>
      if ((lastBlockTs - tx.timestamp).seconds > MaxTimeForUnconfirmed) UnconfirmedTransactionsDatabaseImpl.remove(tx)
    }
  }

  override def genesisData: BlockField[StoredInBlock] = {
    val ipoMembers = List(
      "QTwpq6La1CzXrwbFbcxncb4kv5g2UNFiY2",
      "QSwv4GoR3UjwXKSZN5eoAhHqPcEWTKzomD",
      "QhcAVCVWbWVV1zR3YGFXpbqPdC3f9U9Vk8",
      "QQBe83C4re58s7RPTJADw2uqD8jBubebof",
      "QPsvHmzarLsHDRzjqpGRuLKJMh5WWf6KSp",
      "QN7q77Szn3dXaWXFJigGN7KdyegFEGNj8J",
      "QgN6AT5yrnB12U42QptidCWQkhUEsQaTTp",
      "QNpF56FwQ6xuVynpMndcUEeam3JmPt7tj4",
      "Qa9cNprVTsYPeuxzVjad5p1CvzAezYffdd",
      "QZkTyUfZFLjPiKzQuJNZo4kV959YZkoYe3"
    )

    val timestamp = 0L

    val txs = ipoMembers.map { addr =>
      val recipient = new Account(addr)
      GenesisTransaction(recipient, 1000000000L, timestamp)
    }

    TransactionsBlockField(txs)
  }

  override def isValid(block: Block): Boolean = transactions(block).forall(isValid(_, blockStorage.history.heightOf(block).getOrElse(0) == 1))

  def isValid(transaction: Transaction, isGenesisBlock: Boolean = false): Boolean = transaction match {
    case ptx: PaymentTransaction =>
      ptx.isSignatureValid() && ptx.validate()(this) == ValidationResult.ValidateOke
    case gtx: GenesisTransaction =>
      isGenesisBlock
    case otx: Any =>
      log.error(s"Wrong kind of tx: $otx")
      false
  }
}

object SimpleTransactionModule {
  type StoredInBlock = Seq[Transaction]

  val MaxTimeForUnconfirmed: Duration = 1.hour
  val MaxTransactionsPerBlock = 100: Byte
}