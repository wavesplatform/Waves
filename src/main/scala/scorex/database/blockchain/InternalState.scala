package scorex.database.blockchain

import java.io.File

import org.mapdb.DBMaker
import scorex.account.Account
import scorex.block.Block
import scorex.transaction.Transaction


// Store current balances only, and balances changes within effective balance depth.
// Store transactions for selected accounts only.

// Make design ready for pruning!
// Make possibility of easy switching underlying storage implementation(e.g. from MapDb to Riak)

class InternalState(dataFolder: String) extends StateQuery {
  private val StateHeight = "height"

  private val database = DBMaker.newFileDB(new File(dataFolder + s"/state"))
    .closeOnJvmShutdown()
    .cacheSize(2048)
    .checksumEnable()
    .mmapFileEnableIfSupported()
    .make()

  database.rollback()
  //initial rollback

  private val balances = database.createHashMap("balances").makeOrGet[Account, BigDecimal]()

  private val accountTransactions = database.createHashMap("watchedTxs").makeOrGet[Account, List[Transaction]]()

  def setStateHeight(height: Int): Unit = database.getAtomicVar(StateHeight).set(height)

  def stateHeight(): Int = database.getAtomicVar(StateHeight).get()

  private def transactionsProcessing(block: Block, reversal: Boolean): Unit = {
    val forger = block.generator

    block.transactions.foreach { tx =>
      val changes = tx.balanceChanges()
      changes.foreach { case (accOpt, delta) =>
        //check whether account is watched, add tx to its txs list if so
        accOpt.foreach { acc =>
          val atxs = accountTransactions.get(acc)
          if (atxs != null) {
            accountTransactions.put(acc, tx :: atxs)
          }
        }

        //update balances sheet
        val acc = accOpt.getOrElse(forger)
        val currentBalance = Option(balances.get(acc)).getOrElse(BigDecimal(0))
        val newBalance = if (!reversal) currentBalance + delta else currentBalance - delta
        balances.put(acc, newBalance).ensuring(newBalance >= 0)
      }
    }
    val newHeight = (if (!reversal) stateHeight() + 1 else stateHeight() - 1).ensuring(_ > 0)
    setStateHeight(newHeight)
    database.commit()
  }

  def appendBlock(block: Block): Unit = transactionsProcessing(block, reversal = false)

  def discardBlock(block: Block): Unit = transactionsProcessing(block, reversal = true)


  //todo: confirmations
  override def balance(address: String, confirmations: Int): BigDecimal = {
    val acc = new Account(address)
    val balance = Option(balances.get(acc)).getOrElse(BigDecimal(0))
    balance
  }

  override def accountTransactions(account: Account): Seq[Transaction] =
    Option(accountTransactions.get(account)).getOrElse(List())

  override def stopWatchingAccountTransactions(account: Account): Unit = accountTransactions.remove(account)

  override def watchAccountTransactions(account: Account): Unit = accountTransactions.put(account, List())
}
