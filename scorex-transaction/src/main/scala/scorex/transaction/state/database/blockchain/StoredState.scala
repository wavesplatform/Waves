package scorex.transaction.state.database.blockchain

import java.io.File

import org.mapdb.{DB, DBMaker}
import scorex.account.Account
import scorex.block.Block
import scorex.transaction.LagonakiTransaction
import scorex.transaction.state.LagonakiState
import scorex.utils.ScorexLogging


// Store current balances only, and balances changes within effective balance depth.
// Store transactions for selected accounts only.
// If no datafolder provided, blockchain lives in RAM (intended for tests only)

// todo: Make design ready for pruning!
// todo: Make possibility of easy switching underlying storage implementation(e.g. from MapDb to Riak)

class StoredState(dataFolderOpt: Option[String]) extends LagonakiState with ScorexLogging {
  private val StateHeight = "height"

  private val database: DB = dataFolderOpt match {
    case Some(dataFolder) =>
      val db = DBMaker.newFileDB(new File(dataFolder + s"/state"))
        .closeOnJvmShutdown()
        .cacheSize(2048)
        .checksumEnable()
        .mmapFileEnableIfSupported()
        .make()
      db.rollback() //clear uncommited data from possibly invalid last run
      db
    case None => DBMaker.newMemoryDB().make()
  }

  private val balances = database.createHashMap("balances").makeOrGet[Account, Long]()

  private val accountTransactions = database.createHashMap("watchedTxs").makeOrGet[Account, List[LagonakiTransaction]]()

  def setStateHeight(height: Int): Unit = database.getAtomicVar(StateHeight).set(height)

  def stateHeight(): Int = database.getAtomicVar(StateHeight).get()

  def processBlock(block:Block): Unit = processBlock(block, reversal = false)

  override def processBlock(block: Block, reversal: Boolean): Unit = {
    val balanceChanges = block.transactionModule.transactions(block)
      .foldLeft(block.consensusModule.feesDistribution(block)) { case (changes, atx) => atx match {
      case tx: LagonakiTransaction =>
        tx.balanceChanges().foldLeft(changes) { case (iChanges, (acc, delta)) =>

          //check whether account is watched, add tx to its txs list if so
          val prevTxs: List[LagonakiTransaction] = Option(accountTransactions.get(acc)).getOrElse(List())
          accountTransactions.put(acc, tx :: prevTxs)

          //update balances sheet
          val currentChange = iChanges.getOrElse(acc, 0L)
          val newChange = currentChange + delta
          iChanges.updated(acc, newChange)
        }

      case _ =>
        log.error("Wrong transaction type in pattern-matching")
        changes
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
  }

  def appendBlock(block: Block): Unit = processBlock(block, reversal = false)

  def discardBlock(block: Block): Unit = processBlock(block, reversal = true)

  //todo: confirmations
  override def balance(address: String, confirmations: Int): Long = {
    val acc = new Account(address)
    val balance = Option(balances.get(acc)).getOrElse(0L)
    balance
  }

  override def accountTransactions(account: Account): Seq[LagonakiTransaction] =
    Option(accountTransactions.get(account)).getOrElse(List())

  override def stopWatchingAccountTransactions(account: Account): Unit = accountTransactions.remove(account)

  override def watchAccountTransactions(account: Account): Unit = accountTransactions.put(account, List())

  //initialization
  setStateHeight(0)

  //for debugging purposes only
  override def toString() = {
    import scala.collection.JavaConversions._
    balances.mkString("\n")
  }
}
