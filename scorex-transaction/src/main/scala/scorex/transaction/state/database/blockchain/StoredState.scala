package scorex.transaction.state.database.blockchain

import java.io.{DataInput, DataOutput, File}

import org.mapdb._
import scorex.account.Account
import scorex.block.Block
import scorex.transaction.state.LagonakiState
import scorex.transaction.{LagonakiTransaction, State, Transaction}
import scorex.utils.ScorexLogging

import scala.util.Try


/** Store current balances only, and balances changes within effective balance depth.
  * Store transactions for selected accounts only.
  * If no datafolder provided, blockchain lives in RAM (intended for tests only)
  */

class StoredState(dataFolderOpt: Option[String]) extends LagonakiState with ScorexLogging {

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
        val bytes = tx.bytes()
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


  private val database: DB = dataFolderOpt match {
    case Some(dataFolder) =>
      log.debug("DB loaded from {}", dataFolder)
      val db = DBMaker.fileDB(new File(dataFolder + s"/state"))
        .closeOnJvmShutdown()
        .cacheSize(2048)
        .checksumEnable()
        .fileMmapEnable()
        .make()
      db.rollback() //clear uncommitted data from possibly invalid last run
      db

    case None => DBMaker.memoryDB().make()
  }

  private val StateHeight = "height"

  private val balances = database.hashMap[Account, Long]("balances")

  private val includedTx: HTreeMap[Array[Byte], Array[Byte]] = database.hashMapCreate("segments")
    .keySerializer(Serializer.BYTE_ARRAY)
    .valueSerializer(Serializer.BYTE_ARRAY)
    .makeOrGet()

  private val accountTransactions = database.hashMap(
    "watchedTxs",
    AccSerializer,
    TxArraySerializer,
    null)

  def setStateHeight(height: Int): Unit = database.atomicInteger(StateHeight).set(height)

  def stateHeight(): Int = database.atomicInteger(StateHeight).get()

  override def processBlock(block: Block, reversal: Boolean): Try[State] = Try {
    val trans = block.transactionModule.transactions(block)
    trans foreach { tx =>
      if (!reversal && includedTx.containsKey(tx.signature)) throw new Error("Trying to add transaction twice")
      else if (!reversal) includedTx.put(tx.signature, block.uniqueId)
      else includedTx.remove(tx.signature, block.uniqueId)
    }
    val balanceChanges = trans.foldLeft(block.consensusModule.feesDistribution(block)) { case (changes, atx) =>
      atx match {
        case tx: LagonakiTransaction =>
          tx.balanceChanges().foldLeft(changes) { case (iChanges, (acc, delta)) =>
            //check whether account is watched, add tx to its txs list if so
            val prevTxs = accountTransactions.getOrDefault(acc, Array())
            if (!reversal) accountTransactions.put(acc, Array.concat(Array(tx), prevTxs))
            else accountTransactions.put(acc, prevTxs.filter(t => !(t.signature sameElements tx.signature)))
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
      val newBalance = if (!reversal) balance + delta else balance - delta
      balances.put(acc, newBalance)
    }

    val newHeight = (if (!reversal) stateHeight() + 1 else stateHeight() - 1).ensuring(_ > 0)
    setStateHeight(newHeight)
    database.commit()
    this
  }

  //todo: confirmations
  override def balance(address: String, confirmations: Int): Long = {
    val acc = new Account(address)
    val balance = Option(balances.get(acc)).getOrElse(0L)
    balance
  }

  override def accountTransactions(account: Account): Array[LagonakiTransaction] =
    Option(accountTransactions.get(account)).getOrElse(Array())

  override def stopWatchingAccountTransactions(account: Account): Unit = accountTransactions.remove(account)

  override def watchAccountTransactions(account: Account): Unit = accountTransactions.put(account, Array())


  //initialization
  setStateHeight(0)

  override def included(tx: Transaction): Boolean = includedTx.containsKey(tx.signature)

  //for debugging purposes only
  override def toString = {
    import scala.collection.JavaConversions._
    balances.mkString("\n")
  }
}
