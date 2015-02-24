package database

import java.io.File

import org.mapdb.DBMaker

class Snapshot(height: Int) extends StateQuery {
  private val database = DBMaker.newFileDB(new File(s"/tmp/snapshot$height"))
    .closeOnJvmShutdown()
    .cacheSize(2048)
    .checksumEnable()
    .mmapFileEnableIfSupported()
    .make()

  private val snapshotHeight = database.getAtomicInteger("height")
  private val balancesMap = database.createHashMap("balances").makeOrGet[String, BigDecimal]()

  override def balance(address: String, fromHeight: Int, confirmations: Int): BigDecimal =
    Option(balancesMap.get(address)).getOrElse(BigDecimal(0))
}
