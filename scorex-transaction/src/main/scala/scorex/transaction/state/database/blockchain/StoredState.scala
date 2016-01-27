package scorex.transaction.state.database.blockchain

import java.io.{DataInput, DataOutput, File}

import org.mapdb._
import play.api.libs.json.{JsNumber, JsObject}
import scorex.account.Account
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.transaction.{LagonakiState, LagonakiTransaction, State, Transaction}
import scorex.utils.ScorexLogging

import scala.collection.JavaConversions._
import scala.collection.concurrent.TrieMap
import scala.util.Try


/** Store current balances only, and balances changes within effective balance depth.
  * Store transactions for selected accounts only.
  * If no filename provided, blockchain lives in RAM (intended for tests only).
  *
  * Use apply method of StoredState object to create new instance
  */
class StoredState(database: DB, dbFileName: Option[String]) extends LagonakiState with ScorexLogging {

  private object AccSerializer extends Serializer[Account] {
    override def serialize(dataOutput: DataOutput, a: Account): Unit =
      Serializer.STRING.serialize(dataOutput, a.address)

    override def deserialize(dataInput: DataInput, i: Int): Account = {
      val address = Serializer.STRING.deserialize(dataInput, i)
      new Account(address)
    }
  }

  private object TxArraySerializer extends Serializer[Array[LagonakiTransaction]] {
    override def serialize(dataOutput: DataOutput, txs: Array[LagonakiTransaction]): Unit = {
      DataIO.packInt(dataOutput, txs.length)
      txs.foreach { tx =>
        val bytes = tx.bytes
        DataIO.packInt(dataOutput, bytes.length)
        dataOutput.write(bytes)
      }
    }

    override def deserialize(dataInput: DataInput, i: Int): Array[LagonakiTransaction] = {
      val txsCount = DataIO.unpackInt(dataInput)
      (1 to txsCount).toArray.map { _ =>
        val txSize = DataIO.unpackInt(dataInput)
        val b = new Array[Byte](txSize)
        dataInput.readFully(b)
        LagonakiTransaction.parse(b).get //todo: .get w/out catching
      }
    }
  }

  //Names of collections in DB
  private val StateHeight = "height"
  private val Balances = "balances"
  private val IncludedTx = "includedTx"
  private val WatchedTxs = "watchedTxs"

  private val balances = database.hashMap[Account, Long](Balances)

  private val includedTx: HTreeMap[Array[Byte], Array[Byte]] = database.hashMapCreate(IncludedTx)
    .keySerializer(Serializer.BYTE_ARRAY)
    .valueSerializer(Serializer.BYTE_ARRAY)
    .makeOrGet()

  private val accountTransactions = database.hashMap(
    WatchedTxs,
    AccSerializer,
    TxArraySerializer,
    null)

  override def copyTo(fileNameOpt: Option[String]): State = StoredState.synchronized {
    val db: DB = StoredState.makeDb(fileNameOpt)
    db.atomicInteger(StateHeight).set(stateHeight())
    val balancesCopy = db.hashMap[Account, Long](Balances)

    val includedTxCopy: HTreeMap[Array[Byte], Array[Byte]] = db.hashMapCreate(IncludedTx)
      .keySerializer(Serializer.BYTE_ARRAY)
      .valueSerializer(Serializer.BYTE_ARRAY)
      .makeOrGet()

    val accountTransactionsCopy = db.hashMap(
      WatchedTxs,
      AccSerializer,
      TxArraySerializer,
      null)

    balances.keySet().foreach(key => balancesCopy.put(key, balances(key)))
    includedTx.keySet().foreach(key => includedTxCopy.put(key, includedTx(key)))
    accountTransactions.keySet().foreach(key => accountTransactionsCopy.put(key, accountTransactions(key)))
    db.commit()
    new StoredState(db, None)
  }

  def setStateHeight(height: Int): Unit = database.atomicInteger(StateHeight).set(height)

  //initialization
  if (Option(stateHeight()).isEmpty) setStateHeight(0)

  def stateHeight(): Int = database.atomicInteger(StateHeight).get()

  override def processBlock(block: Block): Try[State] = Try {
    val trans = block.transactions
    trans foreach { tx =>
      if (includedTx.containsKey(tx.signature)) throw new Exception("Already included tx")
      else includedTx.put(tx.signature, block.uniqueId)
    }
    val balanceChanges = trans.foldLeft(block.consensusModule.feesDistribution(block)) { case (changes, atx) =>
      atx match {
        case tx: LagonakiTransaction =>
          tx.balanceChanges().foldLeft(changes) { case (iChanges, (acc, delta)) =>
            //check whether account is watched, add tx to its txs list if so
            val prevTxs = accountTransactions.getOrDefault(acc, Array())
            accountTransactions.put(acc, Array.concat(Array(tx), prevTxs))
            //update balances sheet
            val currentChange = iChanges.getOrElse(acc, 0L)
            val newChange = currentChange + delta
            iChanges.updated(acc, newChange)
          }

        case m =>
          throw new Error("Wrong transaction type in pattern-matching" + m)
      }
    }

    balanceChanges.foreach { case (acc, delta) =>
      val balance = Option(balances.get(acc)).getOrElse(0L)
      val newBalance = balance + delta
      if (newBalance < 0) log.error(s"Account $acc balance $newBalance is negative")
      balances.put(acc, newBalance)
    }

    val newHeight = stateHeight() + 1
    setStateHeight(newHeight)
    database.commit()
    log.debug(s"Total balance at height $newHeight is $totalBalance")

    this
  }

  //todo: confirmations
  override def balance(address: String, confirmations: Int): Long = {
    val acc = new Account(address)
    val balance = Option(balances.get(acc)).getOrElse(0L)
    balance
  }

  def totalBalance: Long = balances.keySet().toList.map(i => balances.get(i)).sum

  override def accountTransactions(account: Account): Array[LagonakiTransaction] =
    Option(accountTransactions.get(account)).getOrElse(Array())

  override def stopWatchingAccountTransactions(account: Account): Unit = accountTransactions.remove(account)

  override def watchAccountTransactions(account: Account): Unit = accountTransactions.put(account, Array())

  override def included(tx: Transaction): Option[BlockId] = Option(includedTx.get(tx.signature))

  //return seq of valid transactions
  def validate(txs: Seq[Transaction]): Seq[Transaction] = {
    val tmpBalances = TrieMap[Account, Long]()

    val r = txs.foldLeft(Seq.empty: Seq[Transaction]) { case (acc, atx) =>
      atx match {
        case tx: LagonakiTransaction =>
          val changes = tx.balanceChanges().foldLeft(Map.empty: Map[Account, Long]) { case (iChanges, (txAcc, delta)) =>
            //update balances sheet
            val currentChange = iChanges.getOrElse(txAcc, 0L)
            val newChange = currentChange + delta
            iChanges.updated(txAcc, newChange)
          }
          val check = changes.forall { a =>
            val balance = tmpBalances.getOrElseUpdate(a._1, balances.get(a._1))
            val newBalance = balance + a._2
            if (newBalance >= 0) tmpBalances.put(a._1, newBalance)
            newBalance >= 0
          }
          if (check) tx +: acc
          else acc
        case _ => acc
      }
    }
    if (r.size == txs.size) r else validate(r)
  }

  def toJson: JsObject = {
    import scala.collection.JavaConversions._
    JsObject(balances.keySet().map(a => a.address -> JsNumber(balances.get(a))).toMap)
  }

  //for debugging purposes only
  override def toString: String = toJson.toString()
}

object StoredState {

  def apply(fileNameOpt: Option[String]): StoredState = new StoredState(makeDb(fileNameOpt), fileNameOpt)

  private[blockchain] def makeDb(DBFileNameOpt: Option[String]) = DBFileNameOpt match {
    case Some(fileName) =>
      DBMaker.fileDB(new File(fileName))
        .closeOnJvmShutdown()
        .cacheSize(2048)
        .checksumEnable()
        .fileMmapEnable()
        .make()
    case None => DBMaker.memoryDB().snapshotEnable().make()
  }
}