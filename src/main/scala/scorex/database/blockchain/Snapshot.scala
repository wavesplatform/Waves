package scorex.database.blockchain

import java.io.File

import org.mapdb.DBMaker
import scorex.account.Account
import scorex.transaction.Transaction

class Snapshot(height: Int) extends StateQuery {
  private val database = DBMaker.newFileDB(new File(s"/tmp/snapshot$height"))
    .closeOnJvmShutdown()
    .cacheSize(2048)
    .checksumEnable()
    .mmapFileEnableIfSupported()
    .make()

  private val snapshotHeight = database.getAtomicInteger("height")
  private val balancesMap = database.createHashMap("balances").makeOrGet[String, BigDecimal]()

  override def balance(address: String, confirmations: Int): BigDecimal =
    Option(balancesMap.get(address)).getOrElse(BigDecimal(0))

  override def accountTransactions(account: Account): Seq[Transaction] = ???

  //todo: implement
  override def watchAccountTransactions(account: Account): Unit = ???

  override def stopWatchingAccountTransactions(account: Account): Unit = ???
}
