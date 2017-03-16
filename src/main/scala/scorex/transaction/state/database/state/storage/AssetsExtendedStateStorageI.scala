package scorex.transaction.state.database.state.storage

trait AssetsExtendedStateStorageI {

  def getIssuanceTransactionsIds(key: String): Seq[String]

  def addTransaction(key: String, transaction: String): Unit

  def setHeight(asset: String, heights: Set[Int]): Unit

  def getHeights(asset: String): Set[Int]

  def addHeight(asset: String, height: Int): Unit

  def getQuantity(key: String): Long

  def setQuantity(key: String, quantity: Long): Unit

  def setReissuable(key: String, reissuable: Boolean): Unit

  def isReissuable(key: String): Boolean

  def removeQuantities(key: String): Unit

  def removeReissuable(key: String): Unit

}
