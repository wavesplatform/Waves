package scorex.transaction.state.database.blockchain

import org.h2.mvstore.{MVMap, MVStore}
import scorex.crypto.encode.Base58
import scorex.transaction._
import scorex.utils.{LogMVMapBuilder, ScorexLogging}

class AssetsExtendedState(db: MVStore) extends ScorexLogging {

  private val heightsTable: MVMap[String, Set[Int]] = db.openMap(AssetsExtendedState.HeightsTableName,
    new LogMVMapBuilder[String, Set[Int]])

  private val quantitiesTable: MVMap[String, Long] = db.openMap(AssetsExtendedState.QuantitiesTableName,
    new LogMVMapBuilder[String, Long])

  private val transactionsTable: MVMap[String, Set[String]] = db.openMap(AssetsExtendedState.TransactionsTableName,
    new LogMVMapBuilder[String, Set[String]])

  private val reissuableTable: MVMap[String, Boolean] = db.openMap(AssetsExtendedState.ReissuableTableName,
    new LogMVMapBuilder[String, Boolean])

  def addAsset(assetId: AssetId, height: Int, transactionId: Array[Byte], quantity: Long, reissuable: Boolean): Unit = {
    val asset = Base58.encode(assetId)
    val transaction = Base58.encode(transactionId)
    val assetAtHeight = s"$asset@$height"
    val assetAtTransaction = s"$asset@$transaction"

    addHeight(asset, height)
    addTransaction(assetAtHeight, transaction)
    addQuantity(assetAtTransaction, quantity)
    setReissuable(assetAtTransaction, reissuable)
  }

  def burnAsset(assetId: AssetId, height: Int, transactionId: Array[Byte], quantity: Long): Unit = {
    require(quantity <= 0, "Quantity of burned asset should be negative")

    val asset = Base58.encode(assetId)
    val transaction = Base58.encode(transactionId)
    val assetAtHeight = s"$asset@$height"
    val assetAtTransaction = s"$asset@$transaction"

    addHeight(asset, height)
    addTransaction(assetAtHeight, transaction)
    addQuantity(assetAtTransaction, quantity)
  }

  def rollbackTo(assetId: AssetId, height: Int): Unit = {
    val asset = Base58.encode(assetId)

    val heights = getHeights(asset)
    val heightsToRemove = heights.filter(h => h > height)
    heightsTable.put(asset, heights -- heightsToRemove)

    val transactionsToRemove: Seq[String] = heightsToRemove.foldLeft(Seq.empty[String]) { (result, h) =>
      result ++ getTransactions(s"$asset@$h")
    }

    val keysToRemove = transactionsToRemove.map(t => s"$asset@$t")

    keysToRemove.foreach { key =>
      quantitiesTable.remove(key)
      reissuableTable.remove(key)
    }
  }

  def getAssetQuantity(assetId: AssetId): Long = {
    val asset = Base58.encode(assetId)
    val heights = getHeights(asset)

    val sortedHeights = heights.toSeq.sorted
    val transactions: Seq[String] = sortedHeights.foldLeft(Seq.empty[String]) { (result, h) =>
      result ++ getTransactions(s"$asset@$h")
    }

    transactions.foldLeft(0L) { (result, transaction) =>
      result + Option(quantitiesTable.get(s"$asset@$transaction")).getOrElse(0L)
    }
  }

  def isReissuable(assetId: AssetId): Boolean = {
    val asset = Base58.encode(assetId)
    val heights = getHeights(asset)

    val reverseSortedHeight = heights.toSeq.reverse
    if (reverseSortedHeight.nonEmpty) {
      val lastHeight = reverseSortedHeight.head
      val transactions = getTransactions(s"$asset@$lastHeight")
      if (transactions.nonEmpty) {
        val transaction = transactions.toSeq.reverse.head
        Option(reissuableTable.get(s"$asset@$transaction")).getOrElse(false)
      } else false
    } else false
  }

  private def getHeights(asset: String): Set[Int] = Option(heightsTable.get(asset)).getOrElse(Set.empty[Int])

  private def addHeight(asset: String, height: Int): Unit = heightsTable.put(asset, getHeights(asset) + height)

  private def getTransactions(key: String): Set[String] =
    Option(transactionsTable.get(key)).getOrElse(Set.empty[String])

  private def addTransaction(key: String, transaction: String): Unit =
    transactionsTable.put(key, getTransactions(key) + transaction)

  private def addQuantity(key: String, quantity: Long): Unit = quantitiesTable.put(key, quantity)

  private def setReissuable(key: String, reissuable: Boolean): Unit = reissuableTable.put(key, reissuable)
}

object AssetsExtendedState {
  val HeightsTableName = "AssetsHeights"
  val TransactionsTableName = "AssetsTransactions"
  val QuantitiesTableName = "AssetsQuantities"
  val ReissuableTableName = "AssetsReissuable"
}
