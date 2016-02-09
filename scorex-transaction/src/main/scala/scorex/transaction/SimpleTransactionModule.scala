package scorex.transaction

import java.io.File

import com.google.common.primitives.{Bytes, Ints}
import play.api.libs.json.{JsObject, Json}
import scorex.account.{Account, PrivateKeyAccount, PublicKeyAccount}
import scorex.app.Application
import scorex.block.{Block, BlockField}
import scorex.crypto.encode.Base58
import scorex.network.message.Message
import scorex.network.{Broadcast, NetworkController, TransactionalMessagesRepo}
import scorex.transaction.LagonakiTransaction.ValidationResult
import scorex.transaction.SimpleTransactionModule.StoredInBlock
import scorex.transaction.state.database.UnconfirmedTransactionsDatabaseImpl
import scorex.transaction.state.database.blockchain.{StoredBlockTree, StoredBlockchain, StoredState}
import scorex.transaction.state.wallet.Payment
import scorex.utils._
import scorex.wallet.Wallet

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.util.{Success, Try}

case class TransactionsBlockField(override val value: Seq[Transaction])
  extends BlockField[Seq[Transaction]] {

  import SimpleTransactionModule.MaxTransactionsPerBlock

  override val name = "transactions"

  override lazy val json: JsObject = Json.obj(name -> Json.arr(value.map(_.json)))

  override lazy val bytes: Array[Byte] = {
    val txCount = value.size.ensuring(_ <= MaxTransactionsPerBlock).toByte
    value.foldLeft(Array(txCount)) { case (bs, tx) =>
      val txBytes = tx.bytes
      bs ++ Bytes.ensureCapacity(Ints.toByteArray(txBytes.length), 4, 0) ++ txBytes
    }
  }
}


class SimpleTransactionModule(implicit val settings: TransactionSettings, application: Application)
  extends TransactionModule[StoredInBlock] with ScorexLogging {

  import SimpleTransactionModule._

  val consensusModule = application.consensusModule
  val networkController = application.networkController

  val TransactionSizeLength = 4
  val InitialBalance = 60000000000L

  private val instance = this

  override val blockStorage = new BlockStorage {
    override val MaxRollback: Int = settings.MaxRollback

    override val history: History = settings.history match {
      case s: String if s.equalsIgnoreCase("blockchain") =>
        new StoredBlockchain(settings.dataDirOpt)(consensusModule, instance)
      case s: String if s.equalsIgnoreCase("blocktree") =>
        new StoredBlockTree(settings.dataDirOpt, MaxRollback)(consensusModule, instance)
      case s =>
        log.error(s"Unknown history storage: $s. Use StoredBlockchain...")
        new StoredBlockchain(settings.dataDirOpt)(consensusModule, instance)
    }


    override val stateHistory: StateHistory = new StateHistory {
      private val stateDir: Option[String] = settings.dataDirOpt

      val cache = TrieMap[String, StoredState]()

      private val StateCopyTimeout = 10.seconds

      private def getFileName(encodedId: String): Option[String] = stateDir.map(f => f + "/state-" + encodedId)

      override def keySet: Set[String] = stateDir match {
        case Some(folder) =>
          new File(folder).listFiles().map(_.getName).toSet.filter(_.startsWith("state-")).map(_.substring(6))
        case None => cache.keySet.toSet
      }

      override def copyState(encodedId: String, state: LagonakiState, toProcess: Block): StoredState = synchronized {
        cache.get(encodedId) match {
          case None =>
            val copy = state.copyTo(getFileName(encodedId)).asInstanceOf[StoredState]
            copy.processBlock(toProcess)
            cache.put(encodedId, copy)
            copy
          case Some(s) =>
            log.warn(s"Trying to copy state $encodedId we already have in cache")
            s
        }
      }

      override def removeState(encodedId: String): Unit = {
        cache.remove(encodedId).foreach(_.database.close())
        getFileName(encodedId).map(new File(_).delete())
      }

      override def state(encodedId: String): Option[StoredState] = state(encodedId, MaxRollback)

      def state(encodedId: String, limit: Int): Option[StoredState] = cache.get(encodedId) match {
        case None =>
          val st: Option[StoredState] = if (!getFileName(encodedId).exists(f => new File(f).exists())) None
          else Try(untilTimeout(StateCopyTimeout)(cache.get(encodedId).get)) match {
            case Success(cachedState) => Some(cachedState)
            case _ =>
              log.info(s"Recover state for block $encodedId from file")
              Some(StoredState(getFileName(encodedId)))
          }

          val recoveredState = if (limit > 0 && (st.isEmpty || !st.get.isValid(InitialBalance))) {
            //State is wrong, recover from the previous one
            log.warn(s"State for block $encodedId} is not valid, recover from parent")
            removeState(encodedId)
            val parenState = history.blockById(encodedId).map(_.referenceField.value)
              .flatMap(id => state(Base58.encode(id), limit - 1))
            parenState.map(s => copyState(encodedId, s, history.blockById(encodedId).get)))
          } else st
          recoveredState.map(s => cache.put(encodedId, s))

          recoveredState

        case ot => ot
      }

      override def state: StoredState = if (history.height() > 0) {
        untilTimeout(StateCopyTimeout)(state(history.lastBlock.encodedId).get)
      } else emptyState

      override val emptyState = StoredState(getFileName(""))
    }
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

  override def formBlockData(transactions: StoredInBlock): TransactionsBlockField = TransactionsBlockField(transactions)

  //TODO asInstanceOf
  override def transactions(block: Block): StoredInBlock =
    block.transactionDataField.asInstanceOf[TransactionsBlockField].value

  override def packUnconfirmed(): StoredInBlock = blockStorage.state.validate(UnconfirmedTransactionsDatabaseImpl.all()
    .filter(isValid).take(MaxTransactionsPerBlock).filter(blockStorage.state.included(_).isEmpty))
    .ensuring(isValid(_, blockStorage.state.asInstanceOf[StoredState]))

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

  override def onNewOffchainTransaction(transaction: Transaction): Unit =
    if (UnconfirmedTransactionsDatabaseImpl.putIfNew(transaction)) {
      val spec = TransactionalMessagesRepo.TransactionMessageSpec
      val ntwMsg = Message(spec, Right(transaction), None)
      networkController ! NetworkController.SendToNetwork(ntwMsg, Broadcast)
    }

  def createPayment(payment: Payment, wallet: Wallet): Option[PaymentTransaction] = {
    wallet.privateKeyAccount(payment.sender).map { sender =>
      createPayment(sender, new Account(payment.recipient), payment.amount, payment.fee)
    }
  }

  def createPayment(sender: PrivateKeyAccount, recipient: Account, amount: Long, fee: Long): PaymentTransaction = {
    val time = NTP.correctedTime()
    val sig = PaymentTransaction.generateSignature(sender, recipient, amount, fee, time)
    val payment = new PaymentTransaction(new PublicKeyAccount(sender.publicKey), recipient, amount, fee, time, sig)
    if (payment.validate()(this) == ValidationResult.ValidateOke) {
      onNewOffchainTransaction(payment)
    }
    payment
  }

  override def genesisData: BlockField[StoredInBlock] = {
    val ipoMembers = List(
      //peer 1 accounts
      "jACSbUoHi4eWgNu6vzAnEx583NwmUAVfS",
      "aptcN9CfZouX7apreDB6WG2cJVbkos881",
      "kVVAu6F21Ax2Ugddms4p5uXz4kdZfAp8g",
      //peer 2 accounts
      "mobNC7SHZRUXDi4GrZP9T2F4iLC1ZidmX",
      "ffUTdmFDesA7NLqLaVfUNgQRD2Xn4tNBp",
      "UR2WjoDCW32XAvYuPbyQW3guxMei5HKf1"
    )

    val timestamp = 0L
    val totalBalance = InitialBalance

    val txs = ipoMembers.map { addr =>
      val recipient = new Account(addr)
      GenesisTransaction(recipient, totalBalance / ipoMembers.length, timestamp)
    }

    TransactionsBlockField(txs)
  }

  override def isValid(block: Block): Boolean = blockStorage.state(block.referenceField.value) match {
    case Some(blockState) =>
      isValid(block.transactions, blockState)
    case None =>
      log.warn(s"No block state ${Base58.encode(block.referenceField.value)} in history")
      false
  }

  //TODO asInstanceOf
  override def isValid(transaction: Transaction): Boolean =
    isValid(transaction, blockStorage.state.asInstanceOf[StoredState])

  private def isValid(transactions: Seq[Transaction], state: State): Boolean =
    transactions.forall(isValid) && state.isValid(transactions)

  private def isValid(transaction: Transaction, txState: StoredState): Boolean = transaction match {
    case tx: PaymentTransaction =>
      val r = tx.signatureValid && tx.validate(txState) == ValidationResult.ValidateOke && txState.included(tx).isEmpty
      if (!r) log.debug(s"Invalid $tx: ${tx.signatureValid}&&${tx.validate(txState)}&&" +
        txState.included(tx).map(Base58.encode))
      r
    case gtx: GenesisTransaction =>
      blockStorage.history.height() == 0
    case otx: Any =>
      log.error(s"Wrong kind of tx: $otx")
      false
  }

}

object SimpleTransactionModule {
  type StoredInBlock = Seq[Transaction]

  val MaxTimeForUnconfirmed = 1.hour
  val MaxTransactionsPerBlock = 100
}
