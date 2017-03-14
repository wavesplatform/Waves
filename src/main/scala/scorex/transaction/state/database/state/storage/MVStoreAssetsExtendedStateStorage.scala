package scorex.transaction.state.database.state.storage

import org.h2.mvstore.{MVMap, MVStore}
import scorex.utils.LogMVMapBuilder

trait MVStoreAssetsExtendedStateStorage extends AssetsExtendedStateStorageI{
  val db: MVStore

  // ============= transactions
  private lazy val TransactionsTableName = "AssetsTransactions"
  private lazy val transactionsTable: MVMap[String, Seq[String]] = db.openMap(TransactionsTableName,
    new LogMVMapBuilder[String, Seq[String]])

  def getIssuanceTransactionsIds(key: String): Seq[String] =
    Option(transactionsTable.get(key)).getOrElse(Seq.empty[String])

  def addTransaction(key: String, transaction: String): Unit =
    transactionsTable.put(key, (getIssuanceTransactionsIds(key) :+ transaction).distinct)


  // ============= heights
  private lazy val HeightsTableName = "AssetsHeights"
  private lazy val heightsTable: MVMap[String, Set[Int]] = db.openMap(HeightsTableName,
    new LogMVMapBuilder[String, Set[Int]])

  def setHeight(asset: String, heights: Set[Int]): Unit = heightsTable.put(asset, heights)

  def getHeights(asset: String): Set[Int] = Option(heightsTable.get(asset)).getOrElse(Set.empty[Int])

  def addHeight(asset: String, height: Int): Unit = heightsTable.put(asset, getHeights(asset) + height)

  // ============= quantities
  private lazy val QuantitiesTableName = "AssetsQuantities"
  private lazy val quantitiesTable: MVMap[String, Long] = db.openMap(QuantitiesTableName,
    new LogMVMapBuilder[String, Long])

  def getQuantity(key: String): Long = Option(quantitiesTable.get(key)).getOrElse(0L)

  def setQuantity(key: String, quantity: Long): Unit = quantitiesTable.put(key, quantity)

  def removeQuantities(key: String): Unit = quantitiesTable.remove(key)

  // ============= reissuable
  private lazy val ReissuableTableName = "AssetsReissuable"
  private lazy val reissuableTable: MVMap[String, Boolean] = db.openMap(ReissuableTableName,
    new LogMVMapBuilder[String, Boolean])

  def setReissuable(key: String, reissuable: Boolean): Unit = if (reissuable) {
    removeReissuable(key)
  } else {
    reissuableTable.put(key, false)
  }

  def isReissuable(key: String): Boolean = Option(reissuableTable.get(key)).getOrElse(true)

  def removeReissuable(key: String): Unit = reissuableTable.remove(key)
}
