package scorex.transaction.state.database.blockchain

import java.io.{DataInput, DataOutput}

import org.mapdb._
import scorex.account.Account
import scorex.block.Block
import scorex.block.Block.BlockId
import scorex.transaction.state.LagonakiState
import scorex.transaction.{LagonakiTransaction, State, Transaction}
import scorex.utils.ScorexLogging

import scala.collection.concurrent.TrieMap
import scala.util.Try


/** Store current balances only, and balances changes within effective balance depth.
  * Store transactions for selected accounts only.
  * If no datafolder provided, blockchain lives in RAM (intended for tests only)
  */

class StoredState(database: DB) extends LagonakiState with ScorexLogging {

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

  private val StateHeight = "height"

  private val balances = database.hashMap[Account, Long]("balances")

  private val includedTx: HTreeMap[Array[Byte], Array[Byte]] = database.hashMapCreate("includedTx")
    .keySerializer(Serializer.BYTE_ARRAY)
    .valueSerializer(Serializer.BYTE_ARRAY)
    .makeOrGet()

  private val accountTransactions = database.hashMap(
    "watchedTxs",
    AccSerializer,
    TxArraySerializer,
    null)

  def setStateHeight(height: Int): Unit = database.atomicInteger(StateHeight).set(height)

  //initialization
  if (Option(stateHeight()).isEmpty) setStateHeight(0)

  def stateHeight(): Int = database.atomicInteger(StateHeight).get()

  override def processBlock(block: Block, reversal: Boolean): Try[State] = Try {
    val trans = block.transactionModule.transactions(block)
    trans foreach { tx =>
      if (!reversal && includedTx.containsKey(tx.signature)) throw new Exception("Already included tx")
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
    if (!reversal) StoredState.history.put(block.uniqueId, database.snapshot())
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


  override def included(tx: Transaction): Option[BlockId] = Option(includedTx.get(tx.signature))

  //for debugging purposes only
  override def toString: String = {
    import scala.collection.JavaConversions._
    balances.mkString("\n")
  }
}

object StoredState {

  val history = TrieMap[BlockId, DB]()

  def apply(id: BlockId): Option[StoredState] = history.get(id).map(new StoredState(_))

}